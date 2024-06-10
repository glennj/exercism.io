(defpackage :yacht
  (:use :cl)
  (:export :score))
(in-package :yacht)

(defun single (dice wanted)
  (* wanted (count wanted dice)))

(defun choice (dice)
  (reduce #'+ dice))

(defun yacht (sorted)
  (if (= (first sorted) (fifth sorted)) 50 0))

(defun four-of-a-kind (sorted)
  (if (or (= (first sorted) (fourth sorted))
          (= (second sorted) (fifth sorted)))
    (* (second sorted) 4)
    0))

(defun full-house (sorted)
  (if (and (or (and (= (first sorted) (second sorted))
                    (= (third sorted) (fifth sorted)))
               (and (= (first sorted) (third sorted))
                    (= (fourth sorted) (fifth sorted))))
           (not (= (first sorted) (fifth sorted))))
    (choice sorted)
    0))

(defun straight (sorted wanted)
  (if (equal sorted wanted) 30 0))

(defun score (dice category)
  "Returns the score of the dice for the given category."
  (case category
    (:ones            (single dice 1))
    (:twos            (single dice 2))
    (:threes          (single dice 3))
    (:fours           (single dice 4))
    (:fives           (single dice 5))
    (:sixes           (single dice 6))
    (:full-house      (full-house (sort dice #'<)))
    (:four-of-a-kind  (four-of-a-kind (sort dice #'<)))
    (:little-straight (straight (sort dice #'<)'(1 2 3 4 5)))
    (:big-straight    (straight (sort dice #'<)'(2 3 4 5 6)))
    (:yacht           (yacht (sort dice #'<)))
    (:choice          (choice dice))
    (otherwise 0)))
