(ns pangram)

(def alphabet-size 26)

(defn ascii-upper? [letter]
  (<= 65 (int letter) 90)) ;; between \A and \Z inclusive

(defn pangram? [word]
  (->> (clojure.string/upper-case word)
       (filter ascii-upper?)
       (distinct)
       (count)
       (= alphabet-size)))
