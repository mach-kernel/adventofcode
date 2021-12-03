(ns advent.days.three
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_3.txt"
       io/resource
       slurp
       s/split-lines))

(def input-by-pos
  "Given bins, interleave indices and group, e.g. [0 1 1] [0 1 2] -> {0 [0] 1 [1] 2 [1]}"
  (->> input
       (mapcat #(interleave (range (count %)) %))
       (partition-all 2)
       (group-by first)))

(defn binseq->int
  [bs]
  (->> bs
       reverse
       (map-indexed #(* (Math/pow 2 %1) %2))
       (reduce + 0)
       int))

(defn gamma-epsilon-reduce
  "Find gamma, epsilon is inverse"
  [[g e] x]
  (let [grouped (->> (get input-by-pos x)
                     (group-by second))
        n0 (count (get grouped \0))
        n1 (count (get grouped \1))
        most (if (> n0 n1)
               0
               1)]
    [(conj g most) (conj e (if (= most 0) 1 0))]))

(defn get-o2-co2
  "Recursively count MSB and drop until we run out of digits or meet problem criteria"
  [input pred]
  (loop [input input
         acc []]
    (let [grouped (->> input
                       (group-by first)
                       ; Keep eating LSB until we run out of bins
                       (reduce-kv #(assoc %1 %2 (map (partial drop 1) %3)) {}))
          n0 (count (get grouped \0))
          n1 (count (get grouped \1))
          keep (if (pred n0 n1)
                 (get grouped \0)
                 (get grouped \1))]
      (cond
        (empty? input) acc
        (= 1 (count input)) (concat acc (map #(Integer/parseInt (str %)) (first input)))
        (= n0 n1 0) acc
        (and (= n0 n1) (= pred <)) (recur (get grouped \0) (conj acc 0))
        (and (= n0 n1) (= pred >)) (recur (get grouped \1) (conj acc 1))
        :else (recur keep (conj acc (if (pred n0 n1) 0 1)))))))

(defn solve
  []
  (->> (count input-by-pos)
       (range)
       (reduce gamma-epsilon-reduce [[] []])
       (map binseq->int)
       (reduce *)))

(defn solve-pt-2
  []
  (let [o2 (get-o2-co2 input <)
        co2 (get-o2-co2 input >)]
    (* (binseq->int o2) (binseq->int co2))))