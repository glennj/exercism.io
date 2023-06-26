(ns scrabble-score
  (:require [clojure.string :as str]))

(defn score-letter [letter]
  (case (str/lower-case letter)
    ("a" "e" "i" "o" "u" "l" "n" "r" "s" "t")  1
                                    ("d" "g")  2
                            ("b" "c" "m" "p")  3
                        ("f" "h" "v" "w" "y")  4
                                        ("k")  5
                                    ("j" "x")  8
                                    ("q" "z") 10
                                               0))

(defn score-word [word]
  (->> word
       (map score-letter)
       (apply +)))
