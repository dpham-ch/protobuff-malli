(ns proto-to-malli.core
  (:require [rubberbuf.core :as rubber]
            [rubberbuf.ast-postprocess :as post]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def default-mapping
  {:double   :double
   :float    :double
   :int32    :int
   :int64    :int
   :uint32   :int
   :uint64   :int
   :sint32   :int
   :sint64   :int
   :fixed32  :int
   :fixed64  :int
   :sfixed32 :int
   :sfixed64 :int
   :bool     :boolean
   :string   :string
   :bytes    :bytes
   :any      :any}) ;; google.protobuf.Any mapping if detected?

(def java-mapping
  {:double   :double
   :float    :float
   :int32    :int
   :int64    :long
   :uint32   :int
   :uint64   :long
   :sint32   :int
   :sint64   :long
   :fixed32  :int
   :fixed64  :long
   :sfixed32 :int
   :sfixed64 :long
   :bool     :boolean
   :string   :string
   :bytes    :bytes
   :any      :any})

(def js-mapping
  {:double   :double
   :float    :double
   :int32    :int
   :int64    :string
   :uint32   :int
   :uint64   :string
   :sint32   :int
   :sint64   :string
   :fixed32  :int
   :fixed64  :string
   :sfixed32 :int
   :sfixed64 :string
   :bool     :boolean
   :string   :string
   :bytes    :string
   :any      :any})

(defn get-mapping [target]
  (case target
    :java java-mapping
    :js js-mapping
    :clojure default-mapping
    default-mapping))

(defn resolve-type [mapping type-str]
  (if (keyword? type-str)
    (get mapping type-str type-str)
    ;; If it's a string, it's a reference to another message/enum
    (let [parts (str/split type-str #"/")
          t (if (> (count parts) 1)
              (keyword (str/join "." (butlast parts)) (last parts))
              (keyword type-str))]
      (if (= t :google.protobuf.Any)
        (get mapping :any :any)
        t))))

(defn transform-options [options]
  (when options
    (reduce (fn [acc [k v]]
              (assoc acc (keyword k) v))
            {}
            options)))

(defn transform-field [mapping field-name field-data]
  (let [{:keys [type context options key-type val-type oneof-fields]} field-data
        k (keyword field-name)
        props (transform-options options)]

    (case context
      :map
      (let [k-type (resolve-type mapping key-type)
            v-type (resolve-type mapping val-type)
            schema [:map-of k-type v-type]]
        (if props
          [k props schema]
          [k schema]))

      :oneof
      (let [orn-fields (mapcat (fn [[name data]]
                                 [(keyword name) (resolve-type mapping (:type data))])
                               oneof-fields)
            schema (into [:orn] orn-fields)]
         [k schema]) ;; Oneof usually doesn't have options on the group itself in Malli mapping, but fields do.
                     ;; Rubberbuf structures oneof as a field container.

      ;; Default case (required, optional, repeated)
      (let [base-type (resolve-type mapping type)
            schema (if (= context :repeated)
                     [:vector base-type]
                     base-type)]
        ;; Check description in options?
        ;; Rubberbuf seems to parse options.
        (if props
          [k props schema]
          [k schema])))))

(defn transform-message [mapping msg-data]
  (let [fields (:fields msg-data)
        ;; Fields is a map "name" -> data.
        ;; We need to sort by field id (:fid) to be deterministic? Or just map.
        ;; Malli map order doesn't strictly matter but nice for testing.
        sorted-fields (sort-by (fn [[_ v]] (:fid v)) fields)
        malli-fields (map (fn [[k v]] (transform-field mapping k v)) sorted-fields)]
    (into [:map] malli-fields)))

(defn transform-enum [enum-data]
  (let [fields (:enum-fields enum-data)
        ;; fields is "NAME" -> {:value 0 ...}
        ;; We want [:enum :NAME ...]
        ;; Wait, Malli :enum is usually values? `[:enum "ZERO" "ONE"]` or `[:enum :ZERO :ONE]`?
        ;; My previous implementation used keywords `[:enum :ZERO :ONE]`.
        ;; Rubberbuf gives strings.
        sorted-fields (sort-by (fn [[_ v]] (:value v)) fields)
        enums (map (fn [[k _]] (keyword k)) sorted-fields)]
    (into [:enum] enums)))

(defn transform-ast [mapping ast-map]
  (reduce (fn [registry [name data]]
            (let [k (resolve-type mapping name)]
              (case (:context data)
                :message (assoc registry k (transform-message mapping data))
                :enum (assoc registry k (transform-enum data))
                :service registry ;; Ignore services for now
                registry)))
          {}
          ast-map))

(defn parse-file
  ([filepath] (parse-file filepath {}))
  ([filepath {:keys [target] :or {target :clojure}}]
   (let [file (io/file filepath)
         dir (.getParent file)
         filename (.getName file)
         ;; Rubberbuf protoc expects paths and files.
         ast (rubber/protoc [dir] [filename])
         ;; Unnest and Mapify
         unnested (post/unnest ast)
         mapified (post/mapify unnested)
         ;; Transform to Malli
         mapping (get-mapping target)
         registry (transform-ast mapping mapified)

         ;; Determine the "main" type key.
         ;; Rubberbuf map keys are qualified "package/Name".
         ;; If we just parsed one file, we usually want the message defined there.
         ;; But mapify gives everything.
         ;; Let's pick the first message from the file we parsed?
         ;; Rubberbuf result is {"filename" [ast]}.
         ;; Mapify merges all.
         ;; We need to find the main message from the AST of the specific file.
         file-ast (get ast filename)
         pkg (some #(when (= (first %) :package) (second %)) file-ast)

         ;; Find top level messages in file-ast
         top-msgs (->> file-ast
                       (filter #(or (= (first %) :message) (= (first %) :enum)))
                       (map second))

         main-msg-name (last top-msgs) ;; Use last one as "main" as per previous logic?

         main-key (if pkg
                    (keyword pkg main-msg-name)
                    (keyword main-msg-name))]

     [:schema {:registry registry} main-key])))

;; Alias parse for backward compat or if used
(defn parse [content & args]
  (throw (ex-info "Parsing from string content not directly supported by Rubberbuf wrapper yet, use parse-file." {})))
