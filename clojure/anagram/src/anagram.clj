(ns anagram
  (:require [clojure.string :as str]))

(declare word-data anagram?)

(defn anagrams-for [cmp-word prospect-list]
  (let [[cmp-upper cmp-key] (word-data cmp-word)
        anagram-test (partial anagram? cmp-upper cmp-key)]
    (filter anagram-test prospect-list)))


(defn word-data [word]
  (let [word-upper (str/upper-case word)
        word-key   (apply str (sort word-upper))]
    [word-upper word-key]))

(defn anagram? [compare-upper compare-key word]
  (let [[up ky] (word-data word)]
    (and (not= compare-upper up)
         (=    compare-key   ky))))
