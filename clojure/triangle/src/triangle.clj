(ns triangle)

(defn- is-valid? [sides]
  (let [[a b c] (sort sides)]
    (and (pos? a)
         (< c (+ a b)))))

(defn- triangle-unique-sides-test [cmp & sides]
  (and (is-valid? sides)
       (-> sides set count cmp)))

(def equilateral? (partial triangle-unique-sides-test #(=  % 1)))
(def isosceles?   (partial triangle-unique-sides-test #(<= % 2)))
(def scalene?     (partial triangle-unique-sides-test #(=  % 3)))
