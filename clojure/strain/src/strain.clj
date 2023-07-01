(ns strain)

(defn retain 
  ([p? coll]
    (retain p? coll []))

  ([p? coll result]
    (if (empty? coll)
      result
      (let [[item & coll] coll]
        (recur p? coll (if (p? item)
                         (conj result item)
                         result))))))

(defn discard [p? coll]
  (retain (complement p?) coll))
