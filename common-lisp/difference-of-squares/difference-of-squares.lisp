(defpackage :difference-of-squares
  (:use :cl)
  (:export :sum-of-squares
           :square-of-sum
           :difference))

(in-package :difference-of-squares)

(defun sum (list) (reduce #'+ list))

(defun range (n) (loop for i from 1 upto n collecting i))

(defun square-of-sum (n)
  "Calculates the square of the sum for a given number."
  (expt (sum (range n)) 2))

(defun sum-of-squares (n)
  "Calculates the sum of squares for a given number."
  (sum (mapcar (lambda (x) (expt x 2)) (range n))))

(defun difference (n)
  "Finds the diff. between the square of the sum and the sum of the squares."
  (abs (- (square-of-sum n) (sum-of-squares n))))
