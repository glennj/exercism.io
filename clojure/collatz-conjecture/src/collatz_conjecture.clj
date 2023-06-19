(ns collatz-conjecture)

;(defn collatz [num] 
;  (if (< num 1)
;    (throw (Exception "positive integers only"))
;    (loop [steps 0 n num]
;      (if (= n 1)
;        steps
;        (recur (inc steps)
;               (if (even? n)
;                 (/ n 2)
;                 (+ n n n 1)))))))

(defn next-num [num]
  (if (even? num)
    (quot num 2)
    (+ num num num 1)))

(defn collatz [num]
  (if (< num 1)
    (throw "positive numbers only")
    (->> (iterate next-num num)
         (take-while #(< 1 %))
         (count))))
