(ns advent23.d01 
  (:require [clojure.string :as str]))

(def ordinals
  "one|two|three|four|five|six|seven|eight|nine")

(def ordinal-re
  (re-pattern ordinals))

(def ordinal-name->ordinal
  (let [os (str/split ordinals #"\|")] 
    (apply assoc {} (interleave os (range 1 10)))))

(def rordinal-re
  (re-pattern (str/reverse ordinals)))

(def rordinal-name->ordinal
  (let [os (str/split (str/reverse ordinals) #"\|")]
    (apply assoc {} (interleave os (reverse (range 1 10))))))

(defn line->decoded
  "Substitute first ordinal match from each side, then take the
   first and last numbers"
 [line] 
 (letfn [(s->numvec
          [s]
          (map parse-long (-> s
                              (str/replace #"[A-Za-z]" "")
                              (str/split #""))))
         (mapper
          [lut]
          #(str (get lut % %)))]
   (let [trimmed (str/trim line)
         reversed (str/reverse trimmed)
         fwd (str/replace-first 
              trimmed ordinal-re (mapper ordinal-name->ordinal))
         bwd (str/replace-first
              reversed rordinal-re (mapper rordinal-name->ordinal))]
     [(first (s->numvec fwd))
      (first (s->numvec bwd))])))

(comment 
  (line->decoded "eightone7threenl7mtxbmkpkzqzljrdk")
  (line->decoded "zoneight234"))

(defn lines->calibration
  [s]
  (let [xf (comp
            (map line->decoded) 
            (map #(apply str %))
            (map parse-long))]
    #_(transduce xf conj '() (str/split-lines s))
    (transduce xf + (str/split-lines s))))

(defn -main
  [filename & _] 
  (println (lines->calibration (slurp filename))))

(comment
  (let [in-lines (slurp "resources/d01_small.txt")]
    (lines->calibration in-lines))
  (let [in-lines (slurp "resources/d01.txt")]
    (lines->calibration in-lines)))
