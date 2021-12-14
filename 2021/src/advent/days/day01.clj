(ns advent.days.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; Read input from JAR rsrc into int-seq
(def input
  (->> "day_1.txt"
       io/resource
       slurp
       s/split-lines
       (map #(Integer/parseInt %))))

(defn sum-sliding-window
  "Generate lazy seq of sliding window sums"
  ([n]
   (sum-sliding-window (vec input) 0 n))
  ([c s f]
   (let [subsum (reduce + (subvec c s f))]
     (if (>= f (count c))
       [subsum]
       (lazy-seq
         (cons subsum
               (sum-sliding-window c (inc s) (inc f))))))))

; O(N) time, O(1) space (excl. seq)
(defn solve
  "Produces tuple of [prev, count of number of measurement increases]"
  ([] (solve input))
  ([numbers]
   (reduce
     (fn [[prev ct] x]
       (if (> x prev)
         [x (inc ct)]
         [x ct]))
     [(Integer/MAX_VALUE) 0]
     numbers)))

(defn solve-pt-2
  "Solve part 2"
  []
  (solve (sum-sliding-window 3)))