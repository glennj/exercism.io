(ns queen-attack
  (:require [clojure.string :as str]))

(def board-size 8)

;; TODO validate queen positions

(defn can-attack [queens]
  (let [[bx by] (get queens :b)
        [wx wy] (get queens :w)]
    (or (zero? (- bx wx))
        (zero? (- by wy))
        (= (abs (- bx wx))
           (abs (- by wy))))))

(declare board-row board-cell)

(defn board-string [queens]
  (loop [board [] x 0]
    (if (= x board-size)
      (str/join board)
      (recur (conj board (board-row queens x))
             (inc x)))))

(defn board-row [queens x]
  (loop [row [] y 0]
    (if (= y board-size)
      (str (str/join " " row) "\n")
      (recur (conj row (board-cell queens [x y]))
             (inc y)))))

(defn board-cell [queens position]
  (condp = position 
    (get queens :b) "B"
    (get queens :w) "W"
    "_"))
