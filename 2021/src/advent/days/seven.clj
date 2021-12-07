(ns advent.days.seven
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_7.txt"
       io/resource
       slurp
       (#(s/split % #","))
       (map #(Integer/parseInt %))))

(defn low-fuel-seq
  "Yield indices by fuel remaining"
  []
  (map
    (fn [crab]
      (keep-indexed #(when (= %2 crab) %1) input))
    (sort (distinct input))))

(defn move-crabs
  [target]
  (->> input
       (map-indexed vector)
       (reduce #(+ %1 (Math/abs (- (last %2) target))) 0)))

(defn move-crabs-pt-2
  [target]
  (->> input
      (map-indexed vector)
      (reduce #(let [acc %1
                     n (Math/abs (- (last %2) target))]
                 (+ acc (/ (* n (+ 1 n)) 2))) 0)))

(defn solve
  []
  (let [pt-1 (->> (low-fuel-seq)
                  (mapcat #(map move-crabs %))
                  (apply min))
        pt-2 (->> (low-fuel-seq)
                  (mapcat #(map move-crabs-pt-2 %))
                  (apply min))]
    [pt-1 pt-2]))
