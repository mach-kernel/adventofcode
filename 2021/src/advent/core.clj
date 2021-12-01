(ns advent.core
  (:gen-class)
  (:require [clojure.string :as s]
            [advent.days.one]))

(defn -main
  [& [day run-fn]]
  (when (empty? day)
    (println "Usage: advent [day-ns-name]")
    (System/exit 1))
  (when-let [f (-> day
                   (s/trim)
                   (#(format "advent.days.%s/%s" % (or run-fn "solve")))
                   (symbol)
                   (resolve))]
    (println (format "Day %s:\n%s" day (f)))))
