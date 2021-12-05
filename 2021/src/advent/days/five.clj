(ns advent.days.five
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
  (let [range-x (range (min x1 x2) (+ 1 (max x1 x2)))
        range-y (range (min y1 y2) (+ 1 (max y1 y2)))]
    ; ignore diags
    (if (= (count range-x) (count range-y))
      []
      (if (> (count range-x) (count range-y))
        (map #(into [] [% (first range-y)]) range-x)
        (map #(into [] [(first range-x) %]) range-y)))))

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
