(ns acronym
  (:require [clojure.string :as str]))

;; Two implementations:
(declare acronym-regex
         acronym-recursive)

(defn acronym
  "Converts phrase to its acronym."
  [phrase]
  ;(acronym-regex phrase)
  (acronym-recursive phrase)
)

;; ==============================================
;; using regular expressions
(defn title-case
  "Upper-case all word-starting letters"
  [phrase]
  (str/replace phrase #"\b\p{Lower}" #(str/upper-case %1)))

(defn acronym-regex [phrase]
  ;; find all upper-case letters that are _preceded_ by 
  ;; the start-of-string or a non-upper-case character.
  (->> (title-case phrase)
       (re-seq #"(?<=^|\P{Upper})\p{Upper}")
       str/join))

;; ==============================================
;; iterating over the characters
(defn upper? [chr] (<= 65 (int chr) 90))   ;; A to Z
(defn lower? [chr] (<= 97 (int chr) 122))  ;; a to z
(defn alpha? [chr] (or (upper? chr) (lower? chr)))

(defn acronym-letter? [chr prev]
  (or
    (and (alpha? chr) (not (alpha? prev)))
    (and (upper? chr) (lower? prev))))

(defn acronym-recursive 
  ([phrase]
    (acronym-recursive (seq phrase) \0 ""))
  ([chrs prev acronym]
    (if (empty? chrs)
      (str/upper-case acronym)
      (let [chr (first chrs)]
        (if (acronym-letter? chr prev)
          (recur (rest chrs) chr (str acronym chr))
          (recur (rest chrs) chr acronym))))))
