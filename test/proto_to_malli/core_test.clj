(ns proto-to-malli.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [proto-to-malli.core :as sut]
            [clojure.edn :as edn]))

(defn test-proto-file [name]
  (let [proto-path (str "resources/" name ".proto")
        edn-path (str "resources/" name ".edn")
        expected (edn/read-string (slurp edn-path))
        actual (sut/parse-file proto-path)]
    (is (= expected actual) (str "Failed for " name))))

(deftest simple-proto-test
  (testing "Parses simple.proto correctly"
    (test-proto-file "simple")))

(deftest scalars-test
  (testing "Parses scalars.proto correctly"
    (test-proto-file "scalars")))

(deftest enum-test
  (testing "Parses enum.proto correctly"
    (test-proto-file "enum")))

(deftest nested-test
  (testing "Parses nested.proto correctly"
    (test-proto-file "nested")))

(deftest repeated-test
  (testing "Parses repeated.proto correctly"
    (test-proto-file "repeated")))

(deftest map-test
  (testing "Parses map.proto correctly"
    (test-proto-file "map")))

(deftest oneof-test
  (testing "Parses oneof.proto correctly"
    (test-proto-file "oneof")))

(deftest any-test
  (testing "Parses any.proto correctly"
    (test-proto-file "any")))
