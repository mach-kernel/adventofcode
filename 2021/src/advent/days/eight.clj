(ns advent.days.eight
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as cs]))

(defn map-line
  [line]
  (->> (s/split line #" \| ")
       (map #(s/split % #" "))
       (map #(map set %))))

; [[chars] [output]]
(def input
  (->> "day_8.txt"
       io/resource
       slurp
       s/split-lines
       (map map-line)))

(comment "
Give each segment a 'stable ID'

  00
1    2
1    2
 3333
4    5
4    5
 6666
")

(def count->ordinal
  {2 1
   4 4
   3 7
   7 8})

(def ordinal->segment
  {0 #{0 1 2 4 5 6}
   1 #{2 5}
   2 #{0 2 3 4 6}
   3 #{0 2 3 5 6}
   4 #{1 2 3 5}
   5 #{0 1 3 5 6}
   6 #{0 1 3 4 5 6}
   7 #{0 2 5}
   8 #{0 1 2 3 4 5 6}
   9 #{0 1 2 3 5 6}})

(defn digits->ordinal
  "Find initial ordinals by count"
  [digits]
  (->> digits
       (map #(get count->ordinal (count %)))
       (zipmap digits)))

(defn find-in-dmap
  "Given digit map, a count, and a segset, find the first digits"
  [dmap n seg]
  (->> dmap
       (filter #(and (cs/superset? (first %) seg)
                     (= n (count (first %)))))
       first
       first))

(defn digit-map->char->seg
  "Go from digit map to char->seg-id in most wild and inefficient way possible"
  [dm]
  (let [ordinal->digits (reduce-kv #(if (int? %3) (assoc %1 %3 %2) %1) {} dm)
        seg-0 (cs/difference (ordinal->digits 7) (ordinal->digits 1))
        seg-25 (ordinal->digits 1)
        seg-13 (cs/difference (ordinal->digits 4) seg-25)

        ; find ordinal 3
        digits-ord-3 (find-in-dmap dm 5 seg-25)
        ordinal->digits (assoc ordinal->digits 3 digits-ord-3)

        ; find more segments from knowing ordinal 3
        seg-36 (cs/difference
                 digits-ord-3
                 seg-0
                 seg-25)
        seg-14 (cs/difference (ordinal->digits 8) (ordinal->digits 3))
        seg-6 (cs/difference seg-36 seg-13)
        seg-3 (cs/difference seg-36 seg-6)
        seg-1 (cs/difference seg-13 seg-3)
        seg-4 (cs/difference seg-14 seg-13)

        ; find ordinal 6
        digits-ord-6 (find-in-dmap dm 6 (cs/union seg-0 seg-1 seg-3 seg-4 seg-6))
        ordinal->digits (assoc ordinal->digits 6 digits-ord-6)

        ; find last segments
        seg-2 (cs/difference (ordinal->digits 8) (ordinal->digits 6))
        seg-5 (cs/difference seg-25 seg-2)]
    {seg-0 0
     seg-1 1
     seg-2 2
     seg-3 3
     seg-4 4
     seg-5 5
     seg-6 6}))

(def resolve-segments
  (comp (fn [cs]
          (reduce-kv #(assoc %1 (first %2) %3) {} cs))
        digit-map->char->seg
        digits->ordinal))

(defn segset->ordinal
  "Given a set for a segment find its ordinal"
  [segset]
  (->> ordinal->segment
       (filter #(and (cs/superset? segset (last %))
                     (= (count segset) (count (last %)))))
       first
       first))

; NOTE: to solve for pt 1, filter out non 1,4,7,8 vals
(defn decipher
  "Decipher a line in the puzzle"
  [[digits output]]
  (let [char->seg (resolve-segments digits)]
    (->> output
         (map #(set (map char->seg %)))
         (map segset->ordinal)
         (s/join ""))))

(defn solve
  []
  (->> input
       (map (comp #(Integer/parseInt %) decipher))
       (reduce +)))
