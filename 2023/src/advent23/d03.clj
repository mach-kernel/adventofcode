(ns advent23.d03
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn crawl-island
  "Beginning from an anchor token, flood-fill island coordinates"
  ([matrix coords]
   (crawl-island matrix coords (atom (set [])) (set [])))
  ([matrix [y x :as c] seen island]
   (if (contains? @seen c)
     island
     (let [_ (swap! seen conj c)
           island (conj island c)
           neighbors [[y (- x 1)] ; left
                      [y (+ x 1)] ; right
                      [(+ y 1) x] ; down
                      [(- y 1) x] ; up
                      [(+ y 1) (+ x 1)]  ; diag tr
                      [(- y 1) (+ x 1)]  ; diag br
                      [(- y 1) (- x 1)]  ; diag bl
                      [(+ y 1) (- x 1)]] ; diag tl
           ; only traverse into neighbors with numbers
           collections (for [[yp xp] neighbors
                             :let [^Character next-char (get-in matrix [yp xp 0])]
                             :when (and next-char (Character/isDigit next-char))]
                         (crawl-island matrix [yp xp] seen island))]
       ; it's possible we have no neighbors, so union the current step
       (set/union (reduce set/union collections) island)))))

(defn deep-indexed
  "Given an n-dim coll, decorate y/x indices"
  [coll]
  (letfn [(labeler [i x]
            (if (sequential? x)
              [i (deep-indexed x)]
              [i x]))]
    (vec (map-indexed labeler coll))))

(defn find-coords
  "Given a matrix, emit a sequence of gear coordinates"
  [pred matrix]
  (for [[y row] (deep-indexed matrix)
        [x [^Character char]] row
        :when (pred char)]
    [y x]))

(defn part-1
  "1. Collect coordinates of token connected islands.
   2. Mask out non island coordinates
   3. Regex out remaining numbers and reduce into sum"
  [s]
  (let [matrix (mapv #(str/split % #"") (str/split-lines s))
        token-pred #(not (or (Character/isDigit ^Character %)
                             (= % \.)))
        valid-mask (->> (find-coords token-pred matrix)
                        (map (partial crawl-island matrix))
                        (reduce set/union))
        nums-str (apply str (for [[y row] (deep-indexed matrix)
                                  [x ch] row]
                              (if (contains? valid-mask [y x])
                                ch
                                ",")))]
    [nums-str (->> (re-seq #"\d+" nums-str)
                   (map parse-long)
                   (reduce +))]))

(defn line->nums-islands
  "Given a map of coords->island label and an input line,
  return a seq of [[num label] ...]"
  [g->l y line]
  (let [num-matcher (re-matcher #"\d+" line)]
    (for [[n start] (->> (repeatedly #(let [num (re-find num-matcher)]
                                        [num (when num
                                               (.start num-matcher))]))
                         (take-while first))
          :let [island-label (get g->l [y start])]
          :when island-label]
      [n island-label])))

(defn part-2
  "1. Label *-connected islands in order they are discovered
   2. Find each number and annotate with its island
   3. Keep all pairs with 2 islands, get their product
   4. Reduce into final sum"
  [s]
  (let [lines (str/split-lines s)
        matrix (mapv #(str/split % #"") lines)
        islands (->> (find-coords #(= % \*) matrix)
                     (map (partial crawl-island matrix)))
        gear-coords->label (->> islands
                                (map-indexed #(zipmap %2 (repeat %1)))
                                (reduce merge))
        nums->island (->> lines
                          (map-indexed (partial line->nums-islands gear-coords->label))
                          (mapcat identity)
                          (group-by last))]
    (->> (vals nums->island)
         (keep #(when (= (count %) 2)
                  (map (comp parse-long first) %)))
         (reduce #(+ %1 (apply * %2)) 0))))

(comment
  (part-1 (slurp "resources/d03_small.txt"))
  (part-1 (slurp "resources/d03.txt"))
  (part-2 (slurp "resources/d03_small.txt"))
  (part-2 (slurp "resources/d03.txt")))
