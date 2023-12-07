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

(defn quad-dist
  [ms record]
  (let [root-pos (long (Math/floor
                         (/ (+ ms (Math/sqrt (- (Math/pow ms 2) (* 4 record))))
                            2)))
        root-neg (long (Math/floor
                         (/ (- ms (Math/sqrt (- (Math/pow ms 2) (* 4 record))))
                            2)))]
    (abs (- root-pos root-neg))))

(defn part-1
  [s]
  (->> (input->race-table s)
       (map #(apply quad-dist %))
       (reduce *)))

(defn part-2
  [s]
  (let [rt (input->race-table s)]
    (->> {(parse-long (apply str (keys rt)))
          (parse-long (apply str (vals rt)))}
         (map #(apply quad-dist %)))))

(comment
  (let [input (slurp "resources/d06.txt")]
    #_(part-1 input)
    (part-2 input)))
