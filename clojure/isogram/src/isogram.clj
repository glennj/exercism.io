(ns isogram)

(declare word-letters)

;; iteration 1: accumulate a set
;; - advantage: early return for non-isograms
;(defn isogram? [word]
;  (loop [letters (word-letters word) seen #{}]
;    (cond
;      (empty? letters) true
;      (contains? seen (first letters)) false
;      :else (recur (rest letters)
;                   (conj seen (first letters))))))

;; iteration 2: take advantage of builtin functionality
(defn isogram? [word]
  (apply distinct? (word-letters word)))

(defn word-letters [word]
  (->> (clojure.string/lower-case word)
       (filter #(Character/isLetter %))))
