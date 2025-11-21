(require '[rubberbuf.core :as rubber])
(require '[clojure.pprint :as pp])

(try
  (let [res (rubber/parse (slurp "resources/simple.proto"))]
    (pp/pprint res))
  (catch Exception e
    (println "Error:" e)))
