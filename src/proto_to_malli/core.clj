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

  FIELD = REPEATED? TYPE IDENTIFIER <'='> NUMBER OPTIONS? <';'>
  REPEATED = 'repeated'

  OPTIONS = <'['> OPTION ( <','> OPTION )* <']'>
  OPTION = IDENTIFIER <'='> CONSTANT
  CONSTANT = #'[^,\\]]+'

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

(def default-mapping
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

(def java-mapping
  {"double"   :double
   "float"    :float
   "int32"    :int
   "int64"    :long
   "uint32"   :int
   "uint64"   :long
   "sint32"   :int
   "sint64"   :long
   "fixed32"  :int
   "fixed64"  :long
   "sfixed32" :int
   "sfixed64" :long
   "bool"     :boolean
   "string"   :string
   "bytes"    :bytes ;; byte[]
   "google.protobuf.Any" :any})

(def js-mapping
  {"double"   :double   ;; Number
   "float"    :double   ;; Number
   "int32"    :int      ;; Number
   "int64"    :string   ;; String
   "uint32"   :int      ;; Number
   "uint64"   :string   ;; String
   "sint32"   :int      ;; Number
   "sint64"   :string   ;; String
   "fixed32"  :int      ;; Number
   "fixed64"  :string   ;; String
   "sfixed32" :int      ;; Number
   "sfixed64" :string   ;; String
   "bool"     :boolean
   "string"   :string
   "bytes"    :string   ;; base64 string
   "google.protobuf.Any" :any})

(defn get-mapping [target]
  (case target
    :java java-mapping
    :js js-mapping
    :clojure default-mapping
    default-mapping))

(defn resolve-type [mapping t]
  (get mapping t (keyword t)))

(defn parse-value [v]
  (cond
    (= v "true") true
    (= v "false") false
    (re-matches #"^\".*\"$" v) (subs v 1 (dec (count v)))
    (re-matches #"^[0-9]+$" v) (Integer/parseInt v)
    :else v))

(defn transform-option [k v]
  [(keyword k) (parse-value v)])

(defn transform-options [& opts]
  (into {} opts))

(defn transform-field
  ([mapping type identifier _number]
   [(keyword identifier) (resolve-type mapping type)])
  ([mapping _repeated type identifier _number]
   [(keyword identifier) [:vector (resolve-type mapping type)]])
  ([mapping type identifier _number options]
    [(keyword identifier) options (resolve-type mapping type)])
  ([mapping _repeated type identifier _number options]
    [(keyword identifier) options [:vector (resolve-type mapping type)]]))

(defn transform-map-field [mapping key-type value-type identifier _number]
  [(keyword identifier) [:map-of (resolve-type mapping key-type) (resolve-type mapping value-type)]])

(defn transform-oneof-field [mapping type identifier _number]
  [(keyword identifier) (resolve-type mapping type)])

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

(defn parse
  ([proto-content] (parse proto-content {}))
  ([proto-content {:keys [target] :or {target :clojure}}]
   (let [parsed (parser proto-content)
         mapping (get-mapping target)]
     (if (insta/failure? parsed)
       parsed
       (insta/transform
        {:FIELD (partial transform-field mapping)
         :MAP_FIELD (partial transform-map-field mapping)
         :ONEOF transform-oneof
         :ONEOF_FIELD (partial transform-oneof-field mapping)
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
         :REPEATED str
         :OPTION transform-option
         :OPTIONS transform-options
         :CONSTANT str}
        parsed)))))

(defn parse-file
  ([filepath] (parse-file filepath {}))
  ([filepath opts]
   (parse (slurp filepath) opts)))
