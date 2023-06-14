(ns roman-numerals)

(defn numerals [n]
  (letfn [(roman [n r]
            (cond
              (>= n 1000) (recur (- n 1000) (str r  "M"))
              (>= n  900) (recur (- n  900) (str r "CM"))
              (>= n  500) (recur (- n  500) (str r  "D"))
              (>= n  400) (recur (- n  400) (str r "CD"))
              (>= n  100) (recur (- n  100) (str r  "C"))
              (>= n   90) (recur (- n   90) (str r "XC"))
              (>= n   50) (recur (- n   50) (str r  "L"))
              (>= n   40) (recur (- n   40) (str r "XL"))
              (>= n   10) (recur (- n   10) (str r  "X"))
              (>= n    9) (recur (- n    9) (str r "IX"))
              (>= n    5) (recur (- n    5) (str r  "V"))
              (>= n    4) (recur (- n    4) (str r "IV"))
              (>= n    1) (recur (- n    1) (str r  "I"))
              :else r))]
    (roman n "")))

