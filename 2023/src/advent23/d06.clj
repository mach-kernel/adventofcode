(ns advent23.d06
  (:require [clojure.string :as str]))

(defn input->race-table
  [s]
  (let [[_ times] (str/split s #"Time:")
        [times _] (str/split-lines times)
        [_ dists] (str/split s #"Distance:")
        [dists _] (str/split-lines dists)
        times (->> (str/split times #" ")
                   (keep parse-long))
        dists (->> (str/split dists #" ")
                   (keep parse-long))]
    (zipmap times dists)))

(defn wait->records
  [ms record]
  (letfn [(race [v]
            (let [d (* v (- ms v))]
              (when (> d record)
                d)))]
    (keep race (range ms))))

(defn part-1
  [s]
  (->> (input->race-table s)
       (map #(count (apply wait->records %)))
       (reduce *)))

(defn part-2
  [s]
  (let [rt (input->race-table s)]
    (->> {(parse-long (apply str (keys rt)))
          (parse-long (apply str (vals rt)))}
         (map #(count (apply wait->records %)))
         (reduce *))))

(comment
  (let [input (slurp "resources/d06.txt")]
    (part-1 input)
    (part-2 input)))
