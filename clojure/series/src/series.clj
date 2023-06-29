(ns series)

(defn slices [string span]
  (if (zero? span)
    [""]
    (->> (partition span 1 string)
         (map clojure.string/join))))

;(defn slices [string span]
;  (cond 
;    (zero? span) [""]
;    :else 
;      (let [cs (vec string), len (count string)]
;        (loop [start 0, end span, result []]
;          (if (> end len)
;            (map clojure.string/join result)
;            (recur (inc start)
;                   (inc end)
;                   (->> (subvec cs start end)
;                        (conj result))))))))

