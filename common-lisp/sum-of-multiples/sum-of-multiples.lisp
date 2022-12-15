(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun multiples-below (factors limit)
  (loop for f in factors
        collect
          (loop for i = f then (+ i f)
                while (< i limit)
                collect i)))

(defun sum (factors limit)
  (let* ((lists-of-multples (multiples-below factors limit))
         (multiples (reduce #'append lists-of-multples))
         (uniq-multiples (remove-duplicates multiples)))
    (reduce #'+ uniq-multiples)))
