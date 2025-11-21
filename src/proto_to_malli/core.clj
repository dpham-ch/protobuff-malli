(ns proto-to-malli.core
  (:require [rubberbuf.core :as rubber]
            [rubberbuf.parse :as rubber-parse]
            [rubberbuf.parse-textformat :as rubber-text]
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
   :any      :any})

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
         [k schema])

      (let [base-type (resolve-type mapping type)
            schema (if (= context :repeated)
                     [:vector base-type]
                     base-type)]
        (if props
          [k props schema]
          [k schema])))))

(defn transform-message [mapping msg-data]
  (let [fields (:fields msg-data)
        sorted-fields (sort-by (fn [[_ v]] (:fid v)) fields)
        malli-fields (map (fn [[k v]] (transform-field mapping k v)) sorted-fields)]
    (into [:map] malli-fields)))

(defn transform-enum [enum-data]
  (let [fields (:enum-fields enum-data)
        sorted-fields (sort-by (fn [[_ v]] (:value v)) fields)
        enums (map (fn [[k _]] (keyword k)) sorted-fields)]
    (into [:enum] enums)))

(defn transform-ast [mapping ast-map]
  (reduce (fn [registry [name data]]
            (let [k (resolve-type mapping name)]
              (case (:context data)
                :message (assoc registry k (transform-message mapping data))
                :enum (assoc registry k (transform-enum data))
                :service registry
                registry)))
          {}
          ast-map))

(defn process-ast [ast mapping main-filename]
  (let [unnested (post/unnest ast)
        mapified (post/mapify unnested)
        registry (transform-ast mapping mapified)

        file-ast (get ast main-filename)
        pkg (some #(when (= (first %) :package) (second %)) file-ast)
        top-msgs (->> file-ast
                      (filter #(or (= (first %) :message) (= (first %) :enum)))
                      (map second))
        main-msg-name (last top-msgs)
        main-key (if pkg
                   (keyword pkg main-msg-name)
                   (keyword main-msg-name))]
    [:schema {:registry registry} main-key]))

(defn parse-file
  ([filepath] (parse-file filepath {}))
  ([filepath {:keys [target] :or {target :clojure}}]
   (let [file (io/file filepath)
         dir (.getParent file)
         filename (.getName file)
         ast (rubber/protoc [dir] [filename])
         mapping (get-mapping target)]
     (process-ast ast mapping filename))))

(defn parse
  "Parses Protobuf definition string to Malli schema."
  ([content] (parse content {}))
  ([content {:keys [target] :or {target :clojure}}]
   (let [ast-vector (rubber-parse/parse content)
         ;; Wrap in a virtual filename to reuse process-ast logic which expects a map {filename ast-vector}
         virtual-filename "input.proto"
         ast-map {virtual-filename ast-vector}
         mapping (get-mapping target)]
     (process-ast ast-map mapping virtual-filename))))

(defn parse-text-format
  "Parses Protobuf TextFormat string to Clojure map."
  [content]
  (rubber-text/parse content))
