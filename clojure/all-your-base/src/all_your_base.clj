(ns all-your-base)

(defn convert [input-base input-digits output-base]
  (cond 
    ;; assertions
    (< input-base  2) nil
    (< output-base 2) nil
    (not-every? #(<= 0 %1 (dec input-base)) input-digits) nil

    :else
      (loop [decimal-value (reduce #(+ (* %1 input-base) %2) 0 input-digits)
             output-digits '()]
        (if (zero? decimal-value)
          (cond
            (empty? input-digits)  '()
            (empty? output-digits) '(0)
            :else                  output-digits)
          (recur (quot decimal-value output-base)
                 (conj output-digits (rem decimal-value output-base)))))))
