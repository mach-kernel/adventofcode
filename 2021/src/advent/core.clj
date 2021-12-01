(ns advent.core
  (:gen-class)
  (:require [clojure.string :as s]
            [advent.days.one]))

(defn -main
  [& [day]]
  (when (empty? day)
    (println "Usage: advent [day-ns-name]")
    (System/exit 1))
  (when-let [f (-> day
                   (s/trim)
                   ((partial format "advent.days.%s/solve"))
                   (symbol)
                   (resolve))]
    (println (format "Day %s:\n%s" day (f)))))
