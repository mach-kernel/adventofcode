(ns advent.days.six
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_6.txt"
       io/resource
       slurp
       s/split-lines
       first
       (#(s/split % #","))
       (map #(Integer/parseInt %))))

(def compact-input
  "My favorite function"
  (->> input
       (group-by identity)
       (reduce-kv #(assoc %1 %2 (count %3)) {})))

(defn reduce-ticks
  "Update tick counter map"
  [acc _]
  (let [tick (reduce-kv #(assoc %1 (dec %2) %3) {} acc)]
    ; e.g. you have to process the zeros in-line with the current
    ; epoch, so they will be -1 after xform
    (if-let [zeros (get tick -1)]
      (-> tick
          (assoc 8 zeros)
          (update 6 #(if % (+ % zeros) zeros))
          (dissoc -1))
      tick)))

(defn solve
  []
  (doseq [n [80 256]]
    (->> (range n)
         (reduce reduce-ticks compact-input)
         (vals)
         (reduce +)
         (println))))
