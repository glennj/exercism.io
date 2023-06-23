(ns etl)

(defn transform [source]
  (letfn [(reducer [result score values]
            (if (empty? values)
              result
              (let [new-key (-> values
                                first
                                clojure.string/lower-case)]
                (recur (assoc result new-key score)
                       score
                       (rest values)))))]
    (reduce-kv reducer '{} source)))

;; TODO, investigate `into` and `for`
