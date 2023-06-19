(ns raindrops
  (:require [clojure.string :as str]))

(defn convert [num]
;  (let [sounds (->> '((3 "Pling") (5 "Plang") (7 "Plong"))
;                    (filter #(zero? (rem num (first %))))
;                    (map second))]
;    (if (empty? sounds)
;      (str num)
;      (str/join sounds))))
  (cond-> nil
    (zero? (rem num 3)) (str "Pling")
    (zero? (rem num 5)) (str "Plang")
    (zero? (rem num 7)) (str "Plong")
    :always             (or (str num))))