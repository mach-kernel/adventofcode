(ns advent23.d09
  (:require [clojure.string :as str]))

(defn ->oasis
  [s]
  (letfn [(parse-line [s]
            (mapv parse-long (str/split s #" ")))]
    (mapv parse-line (str/split-lines s))))

(defn report->diffs
  [nums]
  (if (< (count nums) 2)
    [0]
    (->> (partition 2 1 nums)
         (mapv #(apply - (reverse %))))))

(defn diff-seq
  [nums]
  (if (every? zero? nums)
    [nums]
    (let [diffs (report->diffs nums)]
      (lazy-seq
        (cons nums (diff-seq diffs))))))

(defn predict
  [steps & i]
  (let [i (or i 0)]
    (if (>= i (count steps))
      steps
      (let [prev (last (nth steps (max (dec i) 0)))
            predicted (+ prev (last (nth steps i)))]
        (recur (update steps i conj predicted) (inc i))))))

(defn part-1
  [oasis]
  (let [steps (mapv (comp predict
                          vec
                          reverse
                          diff-seq) oasis)]
    (reduce + (map (comp last last) steps))))

(comment
  (let [input (slurp "resources/d09_small.txt")
        oasis (->oasis input)]
    (part-1 oasis)
    (part-1 (mapv (comp vec reverse) oasis))))
