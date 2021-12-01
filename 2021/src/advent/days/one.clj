(ns advent.days.one
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; Read input from JAR rsrc into int-seq
(def input
  (->> "day_1.txt"
       (io/resource)
       (slurp)
       (s/split-lines)
       (map #(Integer/parseInt %))))

; O(N) time, O(1) space (excl. seq)
(defn solve
  "Produces tuple of [prev, count of number of measurement increases]"
  []
  (reduce
    (fn [[prev ct] x]
      (if (> x prev)
        [x (inc ct)]
        [x ct]))
    [(Integer/MAX_VALUE) 0]
    input))
