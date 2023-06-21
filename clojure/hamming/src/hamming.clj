(ns hamming
;  (:require [clojure.string :as str])
)

(defn distance [strand1 strand2]
;  (when (= (count strand1) (count strand2))
;    (->> (str/split strand1 #"")
;         (map list (str/split strand2 #""))
;         (filter #(not= (first %) (second %)))
;         (count))))

;; strings are collections: > (map = "hello" "world") ;; => (false false false true false)
  (when (= (count strand1) (count strand2))
    (->> (map = strand1 strand2)
         (filter false?)
         (count))))
