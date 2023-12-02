(ns advent23.d02
  (:require [clojure.string :as str]))

(defn -main
  [& argv]
  (println argv))

(comment
  "the game
   
   cubes: red/green/blue
   hides rand cubes in bag
   ")

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

(comment
  ;; part 1
  (let [input (slurp "resources/d02.txt")
        games (->> input
                   str/split-lines
                   (map read-game))
        valid-game? (fn [{:keys [red green blue] :or {red 0 green 0 blue 0}}] 
                      (and (<= red 12)
                           (<= green 13)
                           (<= blue 14)))]
    (->> games
         (filter #(every? valid-game? (last %)))
         (map first)
         (reduce +)))
  
  ;; part 2
  (let [input (slurp "resources/d02.txt")
        games (->> input
                   str/split-lines
                   (map read-game))]
    (->> games
         (map #(apply merge-with max (last %)))
         (map #(reduce * (vals %)))
         (reduce +))))

