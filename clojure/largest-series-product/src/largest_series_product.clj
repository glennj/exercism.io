(ns largest-series-product)

(defn- product [nums] (apply * nums))

(defn largest-product [span digit-string]
  (when (or (neg? span)
            (< (count digit-string) span)
            (not-every? #(Character/isDigit %) digit-string))
    (throw "not a number"))
  (->> digit-string
       (map #(Character/digit % 10))
       (partition span 1)
       (map product)
       (apply max)))
