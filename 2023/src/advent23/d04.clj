(ns advent23.d04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-cards
  [s]
  (letfn [(line->sets [s]
            (let [[_ game] (str/split s #":")
                  [win play] (str/split game #"\|")
                  win-nums (str/split win #" ")
                  play-nums (str/split play #" ")]
              [(set (keep parse-long win-nums))
               (set (keep parse-long play-nums))]))]
    (map line->sets (str/split-lines s))))

(defn part-1
  [s]
  (->> (read-cards s)
       (map #(count (apply set/intersection %)))
       (keep #(when (> % 0)
                (Math/pow 2 (dec %))))
       (reduce +)))

(defn part-2
  [s]
  (letfn [(line->copies [i sets]
            (let [from (inc i)
                  matches (count (apply set/intersection sets))]
              (vec (range from (+ from matches)))))]
    (let [card->copies (->> (read-cards s)
                            (map-indexed line->copies)
                            vec)]
      (loop [nums (range (count card->copies))
             run 0
             i 0]
        (if (>= i (count card->copies))
          run
          (let [fs (frequencies nums)
                expanded (apply concat (repeatedly (get fs i) #(get card->copies i)))
                next-nums (->> nums
                               (filterv #(not (= i %)))
                               (concat expanded))]
            (recur next-nums (+ run (get fs i)) (inc i))))))))

(comment
  (part-1 (slurp "resources/d04_small.txt"))
  (part-2 (slurp "resources/d04.txt")))
