(ns advent23.d07
  (:require [clojure.string :as str]))

; part 1
#_(def card->strength
    (let [cards ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"]]
      (zipmap (reverse cards) (range (count cards)))))

; part 2
(def card->strength
  (let [cards ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J"]]
    (zipmap (reverse cards) (range (count cards)))))

(defn freq->card
  [hand]
  (->> (frequencies hand)
       (group-by last)
       (map (fn [[k v]] [k (mapv first v)]))
       (into {})))

(def type->rank
  (let [types [:five-of-kind
               :four-of-kind
               :full-house
               :three-of-kind
               :two-pair
               :one-pair
               :high-card]]
    (zipmap (reverse types) (range (count types)))))

(defn hand->type
  [hand]
  (let [f->c (freq->card hand)]
    (cond
      (contains? f->c 5) :five-of-kind
      (contains? f->c 4) :four-of-kind
      (and (contains? f->c 3)
           (= 1 (count (get f->c 2)))) :full-house
      (and (contains? f->c 3)
           (not (contains? f->c 2))) :three-of-kind
      (= 2 (count (get f->c 2))) :two-pair
      (= 1 (count (get f->c 2))) :one-pair
      (= (count (set hand))
         (count hand)) :high-card)))

(comment
  (let [high-card ["2" "3" "4" "5" "6"]
        one-pair ["A" "2" "3" "A" "4"]
        two-pair ["2" "3" "4" "3" "2"]
        three-kind ["T" "T" "T" "9" "8"]
        full-house ["2" "3" "3" "3" "2"]
        four-kind ["A" "A" "8" "A" "A"]
        five-kind ["J" "J" "J" "J" "J"]]
    [(hand->type high-card)
     (hand->type one-pair)
     (hand->type two-pair)
     (hand->type three-kind)
     (hand->type full-house)
     (hand->type four-kind)
     (hand->type five-kind)]))

(defn tiebreak-hands
  [a b]
  (let [merged (->> (interleave (map card->strength a)
                                (map card->strength b))
                    (partition-all 2))
        [[sa sb]] (drop-while (fn [[ca cb]]
                                (= ca cb)) merged)]
    (if (> sa sb)
      1
      -1)))

(comment
  (let [a ["3" "3" "3" "3" "2"]
        b ["2" "A" "A" "A" "A"]
        c ["7" "7" "8" "8" "8"]
        d ["7" "7" "7" "8" "8"]]
    (tiebreak-hands a b)
    (tiebreak-hands c d)))

(defn ->hands
  [s]
  (let [->hand #(let [[hand bid] (str/split % #" ")]
                  [(str/split hand #"") (parse-long bid)])]
    (map ->hand (str/split-lines s))))

(defn hand-comparator
  [[a {ra :rank at :type}] [b {rb :rank bt :type}]]
  (cond
    (and (= at bt) (= ra rb)) (tiebreak-hands a b)
    (= ra rb) 0
    (>= ra rb) 1
    :else -1))

(defn part-1
  [s]
  (letfn [(->meta [[hand bid]]
            (let [ht (hand->type hand)]
              [hand {:bid bid :type ht :rank (get type->rank ht)}]))]
    (->> (->hands s)
         (map ->meta)
         (sort hand-comparator)
         (map-indexed (fn [r [_ {:keys [bid]}]]
                        (* (inc r) bid)))
         (reduce +))))

(defn subhand->type
  [hand]
  (let [hand-str (apply str hand)
        c->s (dissoc card->strength "J")]
    (->> (keys c->s)
         (map #(let [new-hand (str/split (str/replace hand-str "J" %) #"")
                     t (hand->type new-hand)]
                 [t (get type->rank t)]))
         (sort-by last)
         last
         first)))

(defn part-2
  [s]
  (letfn [(->meta [[hand bid]]
            (let [ht (if (some #{"J"} hand)
                       (subhand->type hand)
                       (hand->type hand))]
              [hand {:bid bid :type ht :rank (get type->rank ht)}]))]
    (->> (->hands s)
         (map ->meta)
         (sort hand-comparator)
         (map-indexed (fn [r [_ {:keys [bid]}]]
                        (* (inc r) bid)))
         (reduce +))))

(comment
  (let [input (slurp "resources/d07.txt")]
    #_(->hands input)
    (part-2 input)))
