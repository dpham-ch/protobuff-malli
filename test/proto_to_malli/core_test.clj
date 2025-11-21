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

(deftest comments-test
  (testing "Parses comments.proto correctly"
    ;; Rubberbuf might handle comments differently (e.g. attaching to AST or ignoring).
    ;; My transformation logic for comments was specific to my Instaparse implementation.
    ;; Rubberbuf AST might not preserve comments in the same way or `unnest/mapify` might drop them.
    ;; If Rubberbuf doesn't support comments in the mapified output (which has :options but maybe not description),
    ;; this test might fail if I expect {:description ...}.
    ;; I'll verify if I can support it.
    ;; Rubberbuf AST documentation says nothing about comments.
    ;; If it fails, I will remove/adjust the expectation or comment out the test if Rubberbuf doesn't support it yet.
    ;; However, `clojobuf` usually strips comments.
    ;; For now, I will keep the test and see. If I cannot support it with Rubberbuf, I will note it.
    (test-proto-file "comments")))

(deftest reserved-test
  (testing "Parses reserved.proto correctly"
    (test-proto-file "reserved")))

(deftest ignore-test
  (testing "Parses ignore.proto correctly"
    (test-proto-file "ignore")))

(deftest nested-types-test
  (testing "Parses nested_types.proto correctly"
    (test-proto-file "nested_types")))

(deftest dots-test
  (testing "Parses dots.proto correctly"
    (test-proto-file "dots")))

(deftest string-parse-test
  (testing "Parses string content directly"
    (let [content "syntax = \"proto3\"; message FromString { string data = 1; }"
          expected [:schema
                    {:registry
                     {:FromString
                      [:map
                       [:data :string]]}}
                    :FromString]
          actual (sut/parse content)]
      (is (= expected actual)))))

(deftest text-format-test
  (testing "Parses text format correctly"
    (let [content "foo: \"bar\" count: 123"
          expected {"foo" "bar" "count" 123}
          actual (sut/parse-text-format content)]
      (is (= expected actual)))))
