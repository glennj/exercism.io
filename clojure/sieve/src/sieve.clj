(ns sieve)
(declare remove-non-primes)

(defn sieve [limit]
  (->> (range 2 (inc limit))
       set
       (remove-non-primes limit)
       sort))

(defn remove-non-primes [limit candidates]
  (letfn [(remove-multiples-of [n cs]
            (let [step (if (= 2 n) n (* 2 n))
                  ms   (range (* n n) (inc limit) step)]
              (apply disj cs ms)))]

    (loop [c 3
           cs (remove-multiples-of 2 candidates)]
      (if (> (* c c) limit)
        cs
        (recur (+ c 2)
               (if (get cs c)
                 (remove-multiples-of c cs)
                 cs))))))
