(ns advent.days.four
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_4.txt"
       io/resource
       slurp
       s/split-lines
       (filter #(not (empty? %)))))

(def grid-sz 5)
(defrecord Board [rows])

(def draws
  (let [nums (-> input
                 first
                 (s/split #","))]
    (map #(Integer/parseInt %) nums)))

(def boards
  (->> input
       ; skip draw stream
       (drop 1)
       ; flatten to seq of ints for easy strtok ops
       (mapcat #(s/split % #" "))
       (filter #(not (empty? %)))
       (map #(Integer/parseInt %))
       ; boardify
       (partition-all grid-sz)
       (partition-all grid-sz)
       (map #(->Board %))))

(defn column-winning?
  "Does the board have a winning column?"
  [^Board {:keys [rows]} col]
  (or (->> rows
           (map #(nth % col))
           (every? #(= % true)))
      false))

(defn board-winning?
  "Is the board a win?"
  [^Board {:keys [rows] :as board}]
  (or (some #(every? (partial = true) %) rows)
      (->> (range grid-sz)
           (map #(column-winning? board %))
           (some #(= true %)))
      false))

(defn mark-number
  "Mark drawn number"
  [n ^Board {:keys [rows]}]
  (->> rows
       (mapcat identity)
       (map #(if (= n %) true %))
       (partition-all 5)
       (->Board)))

(defn sum-unmarked
  "Find sum of unmarked places"
  [^Board {:keys [rows]}]
  (->> rows
       ; you know, maybe it would have been better to just
       ; serialize everything as a single seq of ints if i
       ; know my partition sz...
       (mapcat identity)
       (filter integer?)
       (reduce +)))

(defn game-seq
  "Seq of progressive game state [[draw win-seq lose-seq]]"
  [boards draws]
  (when-first [draw draws]
    (let [[win lose] (->> boards
                          (map (partial mark-number draw))
                          (group-by board-winning?)
                          (reduce-kv #(assoc %1 (keyword (str %2)) %3) {})
                          ((juxt :true :false)))
          round [draw (into [] win) (into [] lose)]]
      (when-not (empty? lose))
      (if (empty? lose)
        [round]
        (lazy-seq (cons round (game-seq lose (drop 1 draws))))))))

(defn solve
  []
  (let [win-states (->> (game-seq boards draws)
                        (filter #(not-empty (second %))))
        [first-draw [first-win]] (first win-states)
        [last-draw last-wins] (last win-states)]
    (format
      "Pt 1: %d\nPt 2: %d\n"
      (* (sum-unmarked first-win) first-draw)
      (* (sum-unmarked (last last-wins)) last-draw))))
