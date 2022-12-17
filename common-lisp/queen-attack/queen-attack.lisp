(defpackage :queen-attack
  (:use :cl)
  (:export :valid-position-p
           :attackp))

(in-package :queen-attack)

(defun x (coordinates) (car coordinates))
(defun y (coordinates) (cdr coordinates))

(defun valid-position-p (coords)
  (and (<= 0 (x coords) 7)
       (<= 0 (y coords) 7)))

(defun attackp (white-queen black-queen)
  (and (valid-position-p black-queen)
       (valid-position-p white-queen)
       (or (= (x black-queen) (x white-queen))
           (= (y black-queen) (y white-queen))
           (= (abs (- (x black-queen) (x white-queen)))
              (abs (- (y black-queen) (y white-queen)))))))
