(ns proto-to-malli.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [proto-to-malli.core :as sut]
            [clojure.edn :as edn]))

(defn test-proto-file [test-name]
  (let [proto-path (str "resources/" test-name ".proto")
        edn-path (str "resources/" test-name ".edn")
        expected (edn/read-string (slurp edn-path))
        actual (sut/parse-file proto-path)]
    (is (= expected actual) (str "Failed for " test-name))))

(deftest simple-proto-test
  (testing "Parses simple.proto correctly"
    (test-proto-file "simple")))

(deftest scalars-test
  (testing "Parses scalars.proto correctly with default target"
    (test-proto-file "scalars"))

  (testing "Parses scalars.proto correctly with Java target"
    (let [actual (sut/parse-file "resources/scalars.proto" {:target :java})
          registry (:registry (second actual))
          scalars (get registry :Scalars)
          fields (into {} (rest scalars))]
       (is (= :double (:d fields)))
       (is (= :float (:f fields)))
       (is (= :int (:i32 fields)))
       (is (= :long (:i64 fields))) ;; int64 -> :long
       (is (= :long (:u64 fields)))
       (is (= :bytes (:by fields)))))

  (testing "Parses scalars.proto correctly with JS target"
    (let [actual (sut/parse-file "resources/scalars.proto" {:target :js})
          registry (:registry (second actual))
          scalars (get registry :Scalars)
          fields (into {} (rest scalars))]
       (is (= :double (:d fields))) ;; float -> double
       (is (= :int (:i32 fields)))
       (is (= :string (:i64 fields))) ;; int64 -> :string
       (is (= :string (:u64 fields)))
       (is (= :string (:by fields)))))) ;; bytes -> :string

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

(deftest options-test
  (testing "Parses options.proto correctly"
    (test-proto-file "options")))
