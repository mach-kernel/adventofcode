(ns advent23.d08
  (:require [clojure.string :as str]))

(def dir->get
  {"R" last "L" first})

(defn ->map-net
  [s]
  (let [[dir & net] (filter (comp not str/blank?)
                            (str/split-lines s))
        line-re #"(\w+) = \((\w+), (\w+)\)"
        ->pair #(let [[[_ k a b]] (re-seq line-re %)]
                  [k [a b]])
        network (into (array-map) (map ->pair net))
        ; do NOT print me, i am an infinite seq
        dir-tape (apply interleave (->> (str/split dir #"")
                                        (map (comp repeat dir->get))))]
    {:tape-thunk (constantly dir-tape) :network network}))

(defn navigate
  [net [read & tape] dest current steps]
  (let [next-step (read (get net current))]
    (if (= next-step dest)
      (inc steps)
      (recur net tape dest next-step (inc steps)))))

(defn navigate-seq
  ([net tape current]
   (cons current (navigate-seq net tape current 0)))
  ([net [read & tape] current steps]
   (let [next-step (read (get net current))]
     (lazy-seq
       (cons next-step
             (navigate-seq net tape next-step (inc steps)))))))

(defn part-1
  [{:keys [tape-thunk network]}]
  (let [start "AAA" finish "ZZZ"]
    (navigate network (tape-thunk) finish start 0)))

(defn factorize
  [n]
  (loop [n n
         i 2
         factors []]
    (if (> (* i i) n)
      (cons (long n) factors)
      (if (not (zero? (mod n i)))
        (recur n (inc i) factors)
        (recur (Math/floor (/ n i)) i (cons i factors))))))

(defn part-2
  [{:keys [tape-thunk network]}]
  (let [nav-seqs (->> (keys network)
                      (filter #(= \A (last %)))
                      (map #(navigate-seq network (tape-thunk) %))
                      (map (fn [nseq]
                             (take-while #(not= \Z (last %)) nseq)))
                      (map count)
                      (mapcat factorize)
                      set)]
    (apply * nav-seqs)))

(comment
  (let [input (slurp "resources/d08.txt")
        context (->map-net input)]
    #_(part-1 context)
    (part-2 context)))
