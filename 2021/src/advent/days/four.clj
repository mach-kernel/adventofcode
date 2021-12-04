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

(defn reduce-first-winner
  "Yield the first winning board"
  [[boards _] draw]
  (let [applied (map (partial mark-number draw) boards)]
    (if-let [winner (->> applied
                         (filter board-winning?)
                         (first))]
      (reduced [winner draw])
      [applied draw])))

(defn reduce-last-winner
  "Yield the last winning board"
  [[boards _] draw]
  (let [applied (map (partial mark-number draw) boards)
        cats (group-by board-winning? applied)
        win (get cats true)
        lose (get cats false)]
    (if (empty? lose)
      (reduced [(last win) draw])
      [lose draw])))

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

(defn solve
  []
  (let [[win-board last-drawn] (reduce reduce-first-winner [boards 0] draws)]
    (* (sum-unmarked win-board) last-drawn)))

(defn solve-pt-2
  []
  (let [[win-board last-drawn] (reduce reduce-last-winner [boards 0] draws)]
    (* (sum-unmarked win-board) last-drawn)))