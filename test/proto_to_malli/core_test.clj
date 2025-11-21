(ns proto-to-malli.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [proto-to-malli.core :as sut]
            [clojure.edn :as edn]))

(deftest simple-proto-test
  (testing "Parses simple.proto correctly"
    (let [expected (edn/read-string (slurp "resources/simple.edn"))
          actual (sut/parse-file "resources/simple.proto")]
      (is (= expected actual)))))
