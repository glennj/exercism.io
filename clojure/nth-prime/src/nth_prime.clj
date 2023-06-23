(ns nth-prime)

(defn next-prime
  ([primes]
   (next-prime primes (+ 2 (last primes))))

  ([primes candidate]
    (if (some #(zero? (rem candidate %)) primes)
      (recur primes (+ 2 candidate))
      candidate)))

(defn nth-prime
  ([n]
   (if (not (pos? n))
     (throw (IllegalArgumentException. "must be positive"))
     (nth-prime n [2 3])))

  ([n primes]
    (if (>= (count primes) n)
      (get primes (dec n))
      (recur n
             (conj primes (next-prime primes))))))
