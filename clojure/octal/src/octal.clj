(ns octal)

(def radix 8)

(defn to-decimal [s]
  (let [digits (map #(Character/digit % radix) s)]
    (if (some neg? digits)
      0
      (reduce #(-> %1 (* radix) (+ %2)) 0 digits))))
