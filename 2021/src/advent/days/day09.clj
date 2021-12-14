(ns advent.days.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as cs]))

(def map-line
  (comp
    vec
    ; '0' '1' '2' -> 0 1 2
    (fn [s] (map #(Integer/parseInt %) s))
    ; '012' -> '0' '1' '2'
    #(s/split % #"")))

(def input
  (->> "day_9.txt"
       io/resource
       slurp
       s/split-lines
       (map map-line)
       vec))

(defn coord->val
  [[y x]]
  (-> input
      (nth y [])
      (nth x Integer/MAX_VALUE)))

(defn coord->adjacent
  [y x]
  (let [ys (interleave
             [(+ 1 y) (- y 1)] (repeat x))
        xs (interleave
             (repeat y) [(+ 1 x) (- x 1)])]
    (->> (concat ys xs)
         (partition-all 2)
         set
         (map coord->val))))

(defn keep-lowest
  [y x point]
  (when (every? #(< point %) (coord->adjacent y x))
    point))

(defn row->low-points
  [y xs]
  (->> xs
       (keep-indexed (partial keep-lowest y))))

(defn sink->basin
  [y x])

(defn solve
  []
  (let [low-pts (->> input
                     (map-indexed row->low-points)
                     (apply concat))]
    (+ (count low-pts) (reduce + low-pts))))



