(ns proto-to-malli.malli-to-proto-test
  (:require [clojure.test :refer [deftest is testing]]
            [proto-to-malli.core :as ptm]
            [proto-to-malli.malli-to-proto :as mtp]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- slurp-proto [file-name]
  (->> file-name
       io/resource
       slurp
       str/trim))

(defn- round-trip-test [file-name]
  (let [malli-schema (ptm/parse-file (io/resource file-name))
        proto-string (mtp/malli->proto malli-schema)
        expected-proto (slurp-proto file-name)]
    (is (= expected-proto proto-string))))

(deftest round-trip-tests
  (testing "simple.proto"
    (round-trip-test "simple.proto"))

  (testing "nested.proto"
    (round-trip-test "nested.proto"))

  (testing "dots.proto"
    (round-trip-test "dots.proto"))

  (testing "properties.proto"
    (round-trip-test "properties.proto")))
