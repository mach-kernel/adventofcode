(ns advent.days.eight
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn map-line
  [line]
  (->> (s/split line #" \| ")
       (map #(s/split % #" "))
       (map #(map (comp s/join sort) %))))

(def input
  (->> "day_8.txt"
       io/resource
       slurp
       s/split-lines
       (map map-line)))

(def count->ordinal
  {6 [0 6]
   2 1
   5 [2 3 5]
   4 4
   3 7
   7 8})

(defn seg->counts
  [[digits output]]
  (let [chars->ordinal (->> digits
                            (map #(get count->ordinal (count %)))
                            (zipmap digits))]
    (map chars->ordinal output)))

(defn solve-pt-1
  []
  (->> input
        (mapcat seg->counts)
        frequencies
        (#(select-keys % [1 4 7 8]))
        (reduce #(+ %1 (last %2)) 0)))
