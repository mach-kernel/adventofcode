(ns advent.days.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_10.txt"
       io/resource
       slurp
       s/split-lines))

(def scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def open->close
  {\{ \}
   \< \>
   \( \)
   \[ \]})

(def close->open
  (reduce-kv #(assoc %1 %3 %2) {} open->close))

(defn validate
  [stack char]
  (if (contains? open->close char)
    (conj stack char)
    (if-not (contains? close->open char)
      (reduced {:res :invalid})
      (if (= (peek stack) (close->open char))
        (pop stack)
        (reduced {:res :mismatch
                  :char char
                  :score (scores char)})))))

(defn solve
  []
  (->> input
       (map #(reduce validate (list) %))
       (keep #(:score %))
       (reduce + 0)))