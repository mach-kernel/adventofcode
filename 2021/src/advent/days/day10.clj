(ns advent.days.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input
  (->> "day_10.txt"
       io/resource
       slurp
       s/split-lines))

(def first-char-score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def complete-char-score
  {\) 1
   \] 2
   \} 3
   \> 4})

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
                  :score (first-char-score char)})))))

(defn reduce-complete
  [tscore char]
  (-> tscore
      (* 5)
      (+ (complete-char-score char))))

(defn solve
  []
  (let [validated (map #(reduce validate (list) %) input)
        pt-1 (->> validated
                  (keep #(:score %))
                  (reduce + 0))
        completion-scores (->> validated
                               (filter list?)
                               (map #(map open->close %))
                               (map #(reduce reduce-complete 0 %))
                               (sort))
        pt-2 (-> completion-scores
                 (nth (/ (count completion-scores) 2)))]
    [pt-1 pt-2]))
