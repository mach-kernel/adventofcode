(ns advent23.d01
  (:require [clojure.string :as str]))

(def ordinals
  "one|two|three|four|five|six|seven|eight|nine")

(def ordinal-re
  (re-pattern ordinals))

(def rordinal-re
  (re-pattern (str/reverse ordinals)))

(def ordinal-name->ordinal
  (let [os (str/split ordinals #"\|")
        nums (map str (range 1 10))]
    (merge
     (apply assoc {} (interleave os nums))
     (apply assoc {} (interleave (map str/reverse os)
                                 nums)))))

(defn line->decoded
  "Substitute first ordinal match from each side, then take the
   first and last numbers"
  [line]
  (letfn [(->first-num
            [s]
            (-> s
                (str/replace #"[A-Za-z]" "")
                (str/split #"")
                first))
          (replace-first
            [s re]
            (str/replace-first s re #(get ordinal-name->ordinal % %)))]
    (let [trimmed (str/trim line)
          reversed (str/reverse trimmed)
          fwd (replace-first trimmed ordinal-re)
          bwd (replace-first reversed rordinal-re)]
      (parse-long
       (str (->first-num fwd) (->first-num bwd))))))

(comment
  (line->decoded "eightone7threenl7mtxbmkpkzqzljrdk")
  (line->decoded "zoneight234"))

(defn lines->calibration
  [s]
  (reduce + (map line->decoded
                 (str/split-lines s))))

(defn -main
  [filename & _]
  (println (lines->calibration (slurp filename))))

(comment
  (let [in-lines (slurp "resources/d01_small.txt")]
    (lines->calibration in-lines))
  (let [in-lines (slurp "resources/d01.txt")]
    (lines->calibration in-lines)))
