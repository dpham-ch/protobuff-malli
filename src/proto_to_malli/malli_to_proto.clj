(ns proto-to-malli.malli-to-proto
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :as csk]
            [proto-to-malli.core :as ptm]))

;; HELPERS
;; -----------------------------------------------------------------------------
(defn- schema-name
  [k]
  (csk/->PascalCase (name k)))

(defn- format-field
  [field-type field-name field-id]
  (str "  " field-type " " field-name " = " field-id ";"))

(defn- format-message
  [msg-name msg-body]
  (str "message " msg-name " {\n" msg-body "\n}"))

(defn- malli-type->proto-type-str
  [malli-type]
  (ptm/resolve-type
   {:double   "double"
    :int      "int64"
    :boolean  "bool"
    :string   "string"
    :bytes    "bytes"
    :any      "google.protobuf.Any"}
   malli-type))

(defn- get-package-name
  [k]
  (when-let [ns (namespace k)]
    (str "package " ns ";")))

;; REGISTRY
;; -----------------------------------------------------------------------------
(defn- malli-field->proto-field
  [registry [field-name & field-args]]
  (let [field-type (if (map? (first field-args))
                     (second field-args)
                     (first field-args))]
    (cond
      ;; Scalar Type
      (keyword? field-type)
      {:type (malli-type->proto-type-str field-type)}

      ;; Vector Type
      (vector? field-type)
      (let [v-type (first field-type)]
        (if (= :vector v-type)
          {:type    (str "repeated " (malli-type->proto-type-str (second field-type)))}
          (throw (ex-info "Unsupported vector type" {:field-type field-type}))))

      :else
      (throw (ex-info "Unsupported field type" {:field-type field-type})))))

(defn- malli-schema->proto-message
  [registry schema-key]
  (let [[schema-type & schema-args] (get registry schema-key)
        message-name (schema-name schema-key)]
    (if (= :map schema-type)
      (let [proto-fields (->> schema-args
                              (map-indexed (fn [i field]
                                             (let [{:keys [type]} (malli-field->proto-field registry field)
                                                   field-name (csk/->snake_case_string (name (first field)))
                                                   field-id (inc i)]
                                               (format-field type field-name field-id))))
                              (str/join "\n"))]
        (format-message message-name proto-fields))
      (throw (ex-info "Unsupported schema type, we only support :map" {:schema-type schema-type})))))


;; PUBLIC API
;; -----------------------------------------------------------------------------
(defn malli->proto
  "Transforms a Malli schema into a Protobuf definition string."
  [malli-schema]
  (let [[_ {:keys [registry]} main-schema-key] malli-schema
        package-name (get-package-name main-schema-key)
        proto-messages (->> (keys registry)
                            (map #(malli-schema->proto-message registry %))
                            (str/join "\n\n"))]
    (str "syntax = \"proto3\";\n\n"
         (when package-name (str package-name "\n\n"))
         proto-messages)))
