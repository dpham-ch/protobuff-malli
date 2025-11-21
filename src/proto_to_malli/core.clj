(ns proto-to-malli.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def proto-grammar
  "
  PROTO = SYNTAX PACKAGE? MESSAGE*
  SYNTAX = <'syntax'> <'='> <'\"'> 'proto3' <'\"'> <';'>
  PACKAGE = <'package'> DOTTED_IDENTIFIER <';'>
  MESSAGE = <'message'> IDENTIFIER <'{'> FIELD* <'}'>
  FIELD = TYPE IDENTIFIER <'='> NUMBER <';'>
  TYPE = 'string' | 'int32' | 'int64' | 'bool' | 'float' | 'double'
  IDENTIFIER = #'[a-zA-Z_][a-zA-Z0-9_]*'
  DOTTED_IDENTIFIER = #'[a-zA-Z_][a-zA-Z0-9_.]*'
  NUMBER = #'[0-9]+'
  <WHITESPACE> = #'\\s+'
  ")

(def parser
  (insta/parser proto-grammar :auto-whitespace :standard))

(def type-mapping
  {"string" 'string?
   "int32"  'int?
   "int64"  'int?
   "bool"   'boolean?
   "float"  'float?
   "double" 'double?})

(defn transform-field [type identifier _number]
  [(keyword identifier) (get type-mapping type type)])

(defn transform-message [identifier & fields]
  (let [fields-map (into [:map] fields)]
    [(keyword identifier) fields-map]))

(defn transform-package [pkg]
  {:package pkg})

(defn transform-proto [& elements]
  (let [pkg (some #(when (and (map? %) (:package %)) (:package %)) elements)
        messages (filter vector? elements)
        qualify (fn [k] (if pkg (keyword (str pkg "." (name k))) k))
        registry (reduce (fn [acc [k v]]
                           (assoc acc (qualify k) v))
                         {}
                         messages)
        last-message-key (some-> messages last first qualify)]
    [:schema {:registry registry} last-message-key]))

(defn parse [proto-content]
  (let [parsed (parser proto-content)]
    (if (insta/failure? parsed)
      parsed
      (insta/transform
       {:FIELD transform-field
        :MESSAGE transform-message
        :PROTO transform-proto
        :PACKAGE transform-package
        :TYPE str
        :IDENTIFIER str
        :DOTTED_IDENTIFIER str
        :NUMBER str
        :SYNTAX (fn [_] nil)}
       parsed))))

(defn parse-file [filepath]
  (parse (slurp filepath)))
