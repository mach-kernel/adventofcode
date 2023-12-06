(ns advent23.d05
  (:require [clojure.string :as str]))

(defn line->range-mapper
  [s]
  (let [lines (->> (str/split-lines s)
                   (mapv #(mapv parse-long (str/split % #" "))))
        lines-by-src (sort-by #(nth % 1) lines)
        lines-by-end (sort-by (fn [[_ s n]]
                                (+ s n)) lines)
        line-count (count lines)
        [_ lower] (first lines-by-src)
        [_ us un] (last lines-by-end)]
    (fn [x]
      (if (or (< x lower) (>= x (+ us un)))
        x
        (or (loop [i 0]
              (when (< i line-count)
                (let [[d s n] (nth lines i)]
                  (if (and (>= x s) (< x (+ s n)))
                    (+ d (- x s))
                    (recur (inc i))))))
            x)))))

(defn input->maps
  [s]
  (let [[_ seed-nums] (re-find #"seeds: (?<nums>[\d ]+)(?i)" s)
        seeds (mapv parse-long (str/split seed-nums #" "))
        map-matcher (re-matcher #".+map:\n(?<nums>[\d \n]+)" s)
        [seed->soil
         soil->fert
         fert->water
         water->light
         light->temp
         temp->humidity
         humidity->location] (->> (repeatedly #(re-find map-matcher))
                                  (take-while last)
                                  (mapv last)
                                  (mapv line->range-mapper))]
    {:seeds seeds
     :seed->soil seed->soil
     :soil->fert soil->fert
     :fert->water fert->water
     :water->light water->light
     :light->temp light->temp
     :temp->humidity temp->humidity
     :humidity->location humidity->location}))

(defn part-1
  [{:keys [seeds seed->soil soil->fert fert->water water->light
           light->temp temp->humidity humidity->location]}]
  (let [lookup-stack [seed->soil soil->fert fert->water water->light
                      light->temp temp->humidity humidity->location]
        seed->location (apply comp (reverse lookup-stack))]
    (->> seeds
         (map seed->location)
         sort
         first)))

(defn part-2
  [{:keys [seeds seed->soil soil->fert fert->water water->light
           light->temp temp->humidity humidity->location]}]
  (let [lookup-stack [seed->soil soil->fert fert->water water->light
                      light->temp temp->humidity humidity->location]
        seed->location (apply comp (reverse lookup-stack))
        range->smallest-location (fn [[s ofs]]
                                   (let [stop-at (+ s ofs)]
                                     (loop [i s run Long/MAX_VALUE]
                                       (when (zero? (mod i 1000000))
                                         (println "still folding..." s))
                                       (if (>= i stop-at)
                                         run
                                         (recur (inc i)
                                                (min run (seed->location i)))))))]
    (->> (partition-all 2 seeds)
         (pmap range->smallest-location)
         sort
         first)))

(comment
  (let [input (slurp "resources/d05.txt")
        context (input->maps input)]
    (part-1 context)
    (part-2 context)))
