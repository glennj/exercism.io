(ns sum-of-multiples)

(defn multiple? [factors num]
  (boolean (some #(zero? (rem num %)) factors)))

(defn sum-of-multiples [factors limit]
  (->> (range limit)
       (filter (partial multiple? factors))
       (reduce +)))
