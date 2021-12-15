(ns advent.days.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

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
    [[y x] point]))

(defn row->low-points
  [y xs]
  (->> xs
       (keep-indexed (partial keep-lowest y))))

(def basin-count
  (atom 0))

(def basins
  (atom {}))

(defn tag-basins
  [y x prev]
  (let [val (coord->val [y x])]
    (when (and (not (= 9 val))
               (not (= Integer/MAX_VALUE val))
               (not (contains? @basins [y x]))
               (<= 0 y (count input))
               (<= 0 x (count (first input)))
               (> val prev))
      ; mark which basin
      (swap! basins assoc [y x] @basin-count)
      (tag-basins (+ y 1) x val)
      (tag-basins (- y 1) x val)
      (tag-basins y (+ x 1) val)
      (tag-basins y (- x 1) val))))

(defn solve
  []
  (reset! basin-count 0)
  (reset! basins {})

  (let [low-pts (->> input
                     (map-indexed row->low-points)
                     (apply concat))
        pt-1 (+ (count low-pts) (reduce #(+ %1 (last %2)) 0 low-pts))]

    ; tag the basins
    (doseq [[[y x]] low-pts]
      (swap! basin-count inc)
      (tag-basins y x Integer/MIN_VALUE))

    (let [pt-2 (->> @basins
                    (group-by last)
                    (reduce-kv #(assoc %1 %2 (count %3)) {})
                    (sort-by #(* -1 (last %)))
                    (take 3)
                    (reduce #(* %1 (last %2)) 1))]
      [pt-1 pt-2])))
