(ns advent23.d02
  (:require [clojure.string :as str]))

(defn read-game
  [s]
  (letfn [(play->pairs
           [s]
           (->> (str/split s #",")
                (map #(filter (comp not str/blank?)
                              (str/split % #" ")))
                (map (fn [[v k]]
                       {(keyword k) (parse-long v)}))
                (reduce merge)))]
    (let [[game tape] (str/split s #":")
          game (-> game
                   (str/split #"Game ")
                   last
                   parse-long)
          tapes (->> (str/split tape #";")
                     (map play->pairs))]
      [game tapes])))

(defn part-1
  [s]
  (letfn [(valid-game? [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
           (and (<= red 12)
                (<= green 13)
                (<= blue 14)))]
   (->> s
        str/split-lines
        (map read-game)
        (filter #(every? valid-game? (last %)))
        (map first)
        (reduce +))))

(defn part-2
  [s]
  (->> s
       str/split-lines
       (map read-game)
       (map #(apply merge-with max (last %)))
       (map #(reduce * (vals %)))
       (reduce +)))

(comment
  (part-1 (slurp "resources/d02.txt"))
  (part-2 (slurp "resources/d02.txt")))
