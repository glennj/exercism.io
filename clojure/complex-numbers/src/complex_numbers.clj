(ns complex-numbers
  (:require [clojure.math :as math]))

(defn real      [[a _]] a)
(defn imaginary [[_ b]] b)
(defn conjugate [[a b]] [a (math/negate-exact b)])

(defn abs [c] (apply math/hypot c))

(defn add [c1 c2] (mapv + c1 c2))
(defn sub [c1 c2] (mapv - c1 c2))

;; Given m := [a b] and n := [c d]
;; Return (f (* a c) (* b d))
(defn- vec-mul-then [m n f]
  (->> (mapv * m n) (apply f)))

(defn mul [[a b] c2]
  [(vec-mul-then [a b] c2 -)
   (vec-mul-then [b a] c2 +)])

(defn div [[a b] c2]
  (let [real (vec-mul-then [a b] c2 +)
        imag (vec-mul-then [b a] c2 -)
        divisor (vec-mul-then c2 c2 +)]
    (->> [real imag]
         (mapv #(/ % divisor))
         (mapv #(-> % (* 100) (/ 100.0))))))  ;; express as floats rounded to 2 decimal places
