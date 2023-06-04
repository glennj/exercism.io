(ns armstrong-numbers)

(defn digits 
  ([num]
    (if (zero? num)
      []
      (digits num [])))
  ([num ds]
    (if (zero? num)
      ds
      (recur (quot num 10) (conj ds (rem num 10))))))

(defn intpow [n exp]
  (reduce * (repeat exp n)))

(defn armstrong-sum [digits len]
  (reduce + (map #(intpow % len) digits)))

(defn armstrong? [num]
  (let [digits (digits num)
        len (count digits)
        sum (armstrong-sum digits len)]
    (= num sum)))
