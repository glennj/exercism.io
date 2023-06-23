(ns flatten-array)

(defn flatten 
  ([v]
    (flatten v []))

  ([v flattened]
    (if (empty? v)
      flattened
      (let [[head & tail] v]
        (recur tail (cond (nil? head)  flattened
                          (coll? head) (into flattened (flatten head))
                          :else        (conj flattened head)))))))
