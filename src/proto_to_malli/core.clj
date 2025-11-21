(ns proto-to-malli.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def proto-grammar
  "
  PROTO = SYNTAX (IMPORTS | OPTION_STMT)* PACKAGE? (MESSAGE | ENUM | OPTION_STMT)*
  SYNTAX = <'syntax'> <'='> <'\"'> 'proto3' <'\"'> <';'>
  IMPORTS = <'import'> <'\"'> DOTTED_IDENTIFIER <'\"'> <';'>
  OPTION_STMT = <'option'> OPTION_KEY <'='> CONSTANT <';'>
  PACKAGE = <'package'> DOTTED_IDENTIFIER <';'>

  ENUM = <'enum'> IDENTIFIER <'{'> ENUM_FIELD* <'}'>
  ENUM_FIELD = IDENTIFIER <'='> NUMBER <';'>

  MESSAGE = COMMENT? <'message'> IDENTIFIER <'{'> (FIELD | MAP_FIELD | ONEOF | MESSAGE | RESERVED | OPTION_STMT)* <'}'>

  FIELD = COMMENT? REPEATED? TYPE IDENTIFIER <'='> NUMBER OPTIONS? <';'>
  REPEATED = 'repeated'

  RESERVED = <'reserved'> (NUMBER_RANGE | QUOTED_STRING) ( <','> (NUMBER_RANGE | QUOTED_STRING) )* <';'>
  NUMBER_RANGE = NUMBER | NUMBER <'to'> NUMBER
  QUOTED_STRING = #'\"[^\"]*\"'

  OPTIONS = <'['> OPTION ( <','> OPTION )* <']'>
  OPTION = OPTION_KEY <'='> CONSTANT
  OPTION_KEY = IDENTIFIER | <'('> DOTTED_IDENTIFIER <')'>
  CONSTANT = #'[^,\\];]+'

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
  COMMENT = #'//.*'
  WS = #'\\s+'
  ")

(def parser
  (insta/parser proto-grammar :auto-whitespace :WS))

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

(defn dotted-to-keyword [s]
  (if (str/includes? s ".")
    (let [parts (str/split s #"\.")]
      (keyword (str/join "." (butlast parts)) (last parts)))
    (keyword s)))

(defn resolve-type [mapping t]
  (or (get mapping t)
      (dotted-to-keyword t)))

(defn parse-value [v]
  (cond
    (= v "true") true
    (= v "false") false
    (re-matches #"^\".*\"$" v) (subs v 1 (dec (count v)))
    (re-matches #"^[0-9]+$" v) (Integer/parseInt v)
    :else v))

(defn parse-comment [c]
  (str/trim (subs c 2)))

(defn transform-option [k v]
  [(keyword k) (parse-value v)])

(defn transform-options [& opts]
  (into {} opts))

(defn merge-options [base-opts new-opts]
  (if (empty? new-opts)
    base-opts
    (merge base-opts new-opts)))

(defn transform-field
  ([mapping comment? type identifier _number]
   (transform-field mapping comment? nil type identifier _number nil))
  ([mapping comment? _repeated type identifier _number]
   (transform-field mapping comment? _repeated type identifier _number nil))
  ([mapping comment? type identifier _number options]
    (transform-field mapping comment? nil type identifier _number options))
  ([mapping comment? _repeated type identifier _number options]
   (let [desc (when (string? comment?) {:description (parse-comment comment?)})
         opts (merge-options (or options {}) desc)
         key-id (keyword identifier)
         val-type (resolve-type mapping type)
         val-schema (if _repeated [:vector val-type] val-type)]
     (if (seq opts)
       [key-id opts val-schema]
       [key-id val-schema]))))

;; Helper to dispatch based on arity for transform-field is tricky with instaparse transform.
;; I need to unify the args.
;; But instaparse passes arguments as is.
;; The optional parts (COMMENT, REPEATED, OPTIONS) make the arity variable.
;; I will define a variadic function and inspect args.

(defn transform-field-variadic [mapping & args]
  (let [comment (when (str/starts-with? (first args) "//") (first args))
        args (if comment (rest args) args)
        repeated (when (= (first args) "repeated") (first args))
        args (if repeated (rest args) args)
        type (first args)
        identifier (second args)
        _number (nth args 2)
        options (nth args 3 nil)

        desc (when comment {:description (parse-comment comment)})
        opts (merge-options (or options {}) desc)
        key-id (keyword identifier)
        val-type (resolve-type mapping type)
        val-schema (if repeated [:vector val-type] val-type)]
    (if (seq opts)
      [key-id opts val-schema]
      [key-id val-schema])))


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

(defn transform-message [comment? identifier & contents]
  (let [[comment identifier contents] (if (str/starts-with? comment? "//")
                                        [comment? identifier contents]
                                        [nil comment? (cons identifier contents)])

        ;; Filter out ignored items (nil)
        valid-contents (remove nil? contents)

        ;; Separate fields from inner messages
        fields (filter vector? valid-contents)

        ;; Inner messages are already transformed into [k v], need to be returned?
        ;; But transform-message is expected to return [key val] for the map.
        ;; If I have inner messages, they should be part of the registry, not the map?
        ;; This is the hard part. Instaparse constructs tree bottom up.
        ;; A MESSAGE returns a [key schema].
        ;; If there are inner messages, they are in `contents`.
        ;; We want to bubble them up?
        ;; Or, we can return a special structure `{:schema [key val] :inner [...]}`?
        ;; Let's try flattening.

        inner-messages (filter (fn [x] (and (map? x) (:inner x))) valid-contents) ;; hacky check?
        ;; Actually, previously MESSAGE returned [keyword [:map ...]]
        ;; Now it might contain other messages.
        ;; If I change MESSAGE to return `{:name ident :schema schema :inner [...]}` it might be easier.

        ;; Wait, valid-contents contains:
        ;; - Fields: [:key schema]
        ;; - Inner Messages: [key schema] (from recursive transform-message)

        fields-only (filter (fn [x] (keyword? (first x))) fields)

        fields-map (into [:map] fields-only)
        fields-map (if comment
                     (into [:map {:description (parse-comment comment)}] fields-only)
                     fields-map)]

    ;; I need to return something that `transform-proto` can handle.
    ;; `transform-proto` expects a list of message vectors `[key schema]`.
    ;; If I return a list of message vectors, `transform-proto` handles it?
    ;; But MESSAGE is inside MESSAGE.
    ;; If `transform-message` returns a sequence of messages, then the parent `transform-message` will receive them in `contents`.
    ;; So `contents` will have `[:field ...]`, `[:field ...]`, `[:InnerMsg ...]`.
    ;; I need to separate them.

    (let [self-entry [(keyword identifier) fields-map]
          inner-entries (filter (fn [x] (keyword? (first x)))
                                (remove (set fields-only) fields))]
       (cons self-entry inner-entries))))

(defn transform-package [pkg]
  {:package pkg})

(defn transform-proto [& elements]
  (let [pkg (some #(when (and (map? %) (:package %)) (:package %)) elements)

        ;; Elements might be nested lists because of transform-message returning list.
        ;; Flatten one level?
        flat-elements (flatten elements)
        ;; Wait, flatten destroys vectors.
        ;; I need a tree-seq or explicit flattening of lists but keeping vectors.

        normalize (fn normalize [x]
                    (if (and (seq? x) (not (vector? x)))
                      (mapcat normalize x)
                      [x]))

        all-items (mapcat normalize elements)

        types (filter vector? all-items)

        qualify (fn [k]
                  ;; If k has namespace, keep it? Or prepend package?
                  ;; If k is :Outer.Inner, and package is P.
                  ;; :P/Outer.Inner?
                  ;; :foo.bar/Open is already qualified.
                  ;; Current logic: (keyword (str pkg "." (name k)))
                  ;; If k is :Outer, name is "Outer".
                  ;; If k is :Outer.Inner, name is "Outer.Inner".
                  (if pkg
                    (keyword (str pkg "." (name k)))
                    k))

        registry (reduce (fn [acc [k v]]
                           (assoc acc (qualify k) v))
                         {}
                         types)
        ;; Last message is the main one?
        ;; With nested types, the "main" one is the one that wraps them.
        ;; It appears last in the `transform-message` output list usually (self-entry is first, but parent calls come last in proto execution order? No, parser returns list of messages).
        ;; In `transform-proto`, `elements` is list of results of `MESSAGE`.
        ;; `MESSAGE` returns `(self-entry inner...)`.
        ;; So `elements` is `((Msg1 inner...) (Msg2 ...))`.
        ;; The last element of `elements` is the last defined message in file.
        last-type-key (some-> (last elements) first first qualify)]
    [:schema {:registry registry} last-type-key]))

(defn parse
  ([proto-content] (parse proto-content {}))
  ([proto-content {:keys [target] :or {target :clojure}}]
   (let [parsed (parser proto-content)
         mapping (get-mapping target)]
     (if (insta/failure? parsed)
       parsed
       (insta/transform
        {:FIELD (partial transform-field-variadic mapping)
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
         :OPTION_STMT (fn [& _] nil)
         :REPEATED str
         :RESERVED (fn [& _] nil) ;; Ignore reserved
         :OPTION transform-option
         :OPTIONS transform-options
         :OPTION_KEY str
         :QUOTED_STRING str
         :NUMBER_RANGE str
         :CONSTANT str
         :COMMENT str
         :WS (fn [_] nil)}
        parsed)))))

(defn parse-file
  ([filepath] (parse-file filepath {}))
  ([filepath opts]
   (parse (slurp filepath) opts)))
