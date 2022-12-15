(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun multiples-below (factors limit)
  (let ((positive-factors (remove-if #'(lambda (n) (not (plusp n))) factors)))
    (loop for f in positive-factors
          appending
            (loop for i = f then (+ i f)
                  while (< i limit)
                  collecting i)
            into multiples
          finally
            (return (remove-duplicates multiples)))))

(defun sum (factors limit)
  (reduce #'+ (multiples-below factors limit)))
