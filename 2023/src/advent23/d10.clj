(ns advent23.d10
  (:require [clojure.string :as str]))

(defn ->grid
  [s]
  (mapv #(str/split % #"")
        (str/split-lines s)))

(def dir->valid
  {:north #{"|" "F" "7" "S"}
   :south #{"|" "L" "J" "S"}
   :east #{"-" "7" "J" "S"}
   :west #{"-" "F" "L" "S"}})

(def pipe->dir
  {"|" #{:north :south}
   "-" #{:east :west}
   "L" #{:north :east}
   "J" #{:north :west}
   "7" #{:west :south}
   "F" #{:south :east}
   "S" #{:north :south :east :west}})

(defn ->connections
  [grid [y x :as coord]]
  (let [here (get-in grid coord)
        dirs (get pipe->dir here)]
    (for [{:keys [dir coord]} [{:dir :west :coord [y (dec x)]}
                               {:dir :east :coord [y (inc x)]}
                               {:dir :south :coord [(inc y) x]}
                               {:dir :north :coord [(dec y) x]}]
          :let [there (get-in grid coord)]
          :when (and (contains? dirs dir)
                     (contains? (get dir->valid dir) there))]
      coord)))

(defn bfs
  ([grid start]
   (bfs grid [(cons 0 start)] (atom #{}) (atom 0)))
  ([grid [[d y x :as c] & q] seen dist]
   (if (nil? c)
     @dist
     (let [_ (swap! dist max d)
           _ (swap! seen conj [y x])
           visits (for [cxn (->connections grid [y x])
                        :when (not (contains? @seen cxn))]
                    (cons (inc d) cxn))]
       (recur grid (concat q visits) seen dist)))))

(defn part-1
  [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :let [c (get-in grid [y x])]
        :when (= c "S")]
    (bfs grid [y x])))

(comment
  (let [input (slurp "resources/d10.txt")
        grid (->grid input)]
    (part-1 grid)))
