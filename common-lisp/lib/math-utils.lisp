;;;; Some utility functions for solving Exercism exercises

(defpackage :math-utils
  (:use :cl)
  (:export :factors
           :mmi))

(in-package :math-utils)

(defun factors (n &optional (fact (floor (sqrt n))) (factors '()))
  "the factors of a number `n` not including the number itself"
  (if (= 1 fact)
    (remove n (remove-duplicates (append factors (list 1 n))))
    (multiple-value-bind (q r) (floor n fact)
      (factors n
               (1- fact)
               (append factors (when (zerop r) (list fact q)))))))

(defun mmi (a m) 
  "Modular Multiplicative Inverse: what is `i` when `(a * i) mod m == 1`"
  (loop for i from 1 upto m
        when (= 1 (mod (* a i) m))
        do (return i)))
