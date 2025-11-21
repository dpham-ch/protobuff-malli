(ns proto-to-malli.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def proto-grammar
  "
  PROTO = SYNTAX IMPORTS* PACKAGE? (MESSAGE | ENUM)*
  SYNTAX = <'syntax'> <'='> <'\"'> 'proto3' <'\"'> <';'>
  IMPORTS = <'import'> <'\"'> DOTTED_IDENTIFIER <'\"'> <';'>
  PACKAGE = <'package'> DOTTED_IDENTIFIER <';'>

  ENUM = <'enum'> IDENTIFIER <'{'> ENUM_FIELD* <'}'>
  ENUM_FIELD = IDENTIFIER <'='> NUMBER <';'>

  MESSAGE = <'message'> IDENTIFIER <'{'> (FIELD | MAP_FIELD | ONEOF)* <'}'>

  FIELD = REPEATED? TYPE IDENTIFIER <'='> NUMBER <';'>
  REPEATED = 'repeated'

  MAP_FIELD = <'map'> <'<'> KEY_TYPE <','> TYPE <'>'> IDENTIFIER <'='> NUMBER <';'>
  KEY_TYPE = 'int32' | 'int64' | 'uint32' | 'uint64' | 'sint32' | 'sint64' |
             'fixed32' | 'fixed64' | 'sfixed32' | 'sfixed64' | 'bool' | 'string'

  ONEOF = <'oneof'> IDENTIFIER <'{'> ONEOF_FIELD* <'}'>
  ONEOF_FIELD = TYPE IDENTIFIER <'='> NUMBER <';'>

  TYPE = SCALAR | DOTTED_IDENTIFIER
  SCALAR = 'double' | 'float' | 'int32' | 'int64' | 'uint32' | 'uint64' |
           'sint32' | 'sint64' | 'fixed32' | 'fixed64' | 'sfixed32' | 'sfixed64' |
           'bool' | 'string' | 'bytes'

  IDENTIFIER = #'[a-zA-Z_][a-zA-Z0-9_]*'
  DOTTED_IDENTIFIER = #'[a-zA-Z_][a-zA-Z0-9_./]*'
  NUMBER = #'[0-9]+'
  <WHITESPACE> = #'\\s+'
  ")

(def parser
  (insta/parser proto-grammar :auto-whitespace :standard))

(def type-mapping
  {"double"   :double
   "float"    :double
   "int32"    :int
   "int64"    :int
   "uint32"   :int
   "uint64"   :int
   "sint32"   :int
   "sint64"   :int
   "fixed32"  :int
   "fixed64"  :int
   "sfixed32" :int
   "sfixed64" :int
   "bool"     :boolean
   "string"   :string
   "bytes"    :bytes
   "google.protobuf.Any" :any})

(defn resolve-type [t]
  (get type-mapping t (keyword t)))

(defn transform-field
  ([type identifier _number]
   [(keyword identifier) (resolve-type type)])
  ([_repeated type identifier _number]
   [(keyword identifier) [:vector (resolve-type type)]]))

(defn transform-map-field [key-type value-type identifier _number]
  [(keyword identifier) [:map-of (resolve-type key-type) (resolve-type value-type)]])

(defn transform-oneof-field [type identifier _number]
  [(keyword identifier) (resolve-type type)])

(defn transform-oneof [identifier & fields]
  [(keyword identifier) (into [:orn] (mapcat identity fields))])

(defn transform-enum-field [identifier _number]
  (keyword identifier))

(defn transform-enum [identifier & fields]
  [(keyword identifier) (into [:enum] fields)])

(defn transform-message [identifier & fields]
  (let [fields-map (into [:map] fields)]
    [(keyword identifier) fields-map]))

(defn transform-package [pkg]
  {:package pkg})

(defn transform-proto [& elements]
  (let [pkg (some #(when (and (map? %) (:package %)) (:package %)) elements)
        types (filter vector? elements)
        qualify (fn [k] (if pkg (keyword (str pkg "." (name k))) k))
        registry (reduce (fn [acc [k v]]
                           (assoc acc (qualify k) v))
                         {}
                         types)
        last-type-key (some-> types last first qualify)]
    [:schema {:registry registry} last-type-key]))

(defn parse [proto-content]
  (let [parsed (parser proto-content)]
    (if (insta/failure? parsed)
      parsed
      (insta/transform
       {:FIELD transform-field
        :MAP_FIELD transform-map-field
        :ONEOF transform-oneof
        :ONEOF_FIELD transform-oneof-field
        :ENUM_FIELD transform-enum-field
        :ENUM transform-enum
        :MESSAGE transform-message
        :PROTO transform-proto
        :PACKAGE transform-package
        :TYPE str
        :KEY_TYPE str
        :SCALAR str
        :IDENTIFIER str
        :DOTTED_IDENTIFIER str
        :NUMBER str
        :SYNTAX (fn [_] nil)
        :IMPORTS (fn [& _] nil)
        :REPEATED str}
       parsed))))

(defn parse-file [filepath]
  (parse (slurp filepath)))
