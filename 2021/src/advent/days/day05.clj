(ns advent.days.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; Go from x1,y1 -> x2,y2 to parsed 4-tuple per row
(def input
  (->> "day_5.txt"
       io/resource
       slurp
       s/split-lines
       (mapcat #(s/split % #" -> "))
       (mapcat #(s/split % #","))
       (map #(Integer/parseInt %))
       (partition-all 4)))

(defn line->points
  "Expand a line entry into its points"
  [x1 y1 x2 y2]
  (let [range-x (if (<= x1 x2)
                  (range x1 (+ 1 x2))
                  (range x1 (- x2 1) -1))
        range-y (if (<= y1 y2)
                  (range y1 (+ 1 y2))
                  (range y1 (- y2 1) -1))
        sz-x (count range-x)
        sz-y (count range-y)]
    (cond
      ; Yield [] for = clause to solve pt 1 again.
      (= sz-x sz-y) (map #(into [] [%1 %2]) range-x range-y)
      (> sz-x sz-y) (map #(into [] [% (first range-y)]) range-x)
      (< sz-x sz-y) (map #(into [] [(first range-x) %]) range-y))))

(defn mark-points
  "Reduces marked points into map m from k [x y]"
  [m k]
  (if (contains? m k)
    (update-in m [k] inc)
    (assoc m k 1)))

(defn solve
  []
  (->> input
       (mapcat #(apply line->points %))
       (reduce mark-points {})
       (filter #(>= (last %) 2))
       count))
