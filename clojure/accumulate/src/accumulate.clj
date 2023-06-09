(ns accumulate)

(defn accumulate 
  ([f v]
    (accumulate f v []))
  ([f v result]
    (if (empty? v)
        result
        (let [hd (first v)
              tl (rest v)
              new-val (f hd)]
          (accumulate f tl (conj result new-val))))))
