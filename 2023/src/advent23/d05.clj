(ns advent23.d05
  (:require
    [clojure.core.reducers :as r]
    [clojure.string :as str]))

(defn line->range-mapper
  [s]
  (let [lines (->> (str/split-lines s)
                   (mapv #(mapv parse-long (str/split % #" "))))
        lines-by-src (sort-by #(nth % 1) lines)
        lines-by-end (sort-by (fn [[_ s n]]
                                (+ s n)) lines)
        [_ lower] (first lines-by-src)
        [_ us un] (last lines-by-end)]
    (fn [x]
      (if (or (< x lower) (> x (+ us un)))
        x
        (or (first (for [[d s n] lines
                         :let [res (+ d (- x s))]
                         :when (<= s x (+ s n))]
                     res))
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
                                   #_(println "folding..." s)
                                   (reduce #(min %1 (seed->location %2))
                                           Long/MAX_VALUE (range s (+ s ofs))))]
    (->> (partition-all 2 seeds)
         (sort-by first)
         (pmap range->smallest-location)
         sort
         first)))

(comment
  (let [input (slurp "resources/d05_small.txt")
        context (input->maps input)]
    #_(part-1 context)
    (part-2 context)))

(comment
  (let [min-fold (fn
                   ([] (Long/MAX_VALUE))
                   ([a b] (min a (seed->location b))))]
    #_(r/fold min-fold (vec %))))

