(ns prime-factors)

(defn- factorize [n f fs]
  (if (> (* f f) n) 
    (if (= 1 n)
      fs
      (conj fs n))
    (if (zero? (rem n f))
      (recur (quot n f) f (conj fs f))
      (recur n (inc f) fs))))

(defn of [n]
  (factorize n 2 []))
