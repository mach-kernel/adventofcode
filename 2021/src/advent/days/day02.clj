(ns advent.days.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_2.txt"
       io/resource
       slurp
       s/split-lines
       (map #(let [[dir n] (s/split % #" ")]
               [dir (Integer/parseInt n)]))))

(defn sum-dir
  [[y x] [dir n]]
  (case dir
    "forward" [y (+ x n)]
    "up" [(- y n) x]
    "down" [(+ y n) x]))

(defn sum-dir-pt-2
  [[y a x] [dir n]]
  (case dir
    "forward" [(+ y (* a n))
               a
               (+ x n)]
    "up" [y
          (- a n)
          x]
    "down" [y
            (+ a n)
            x]))

(defn solve
  []
  (->> input
       (reduce sum-dir [0 0])
       (reduce *)))

(defn solve-pt-2
  []
  (let [[y _ x] (reduce sum-dir-pt-2 [0 0 0] input)]
    (* y x)))