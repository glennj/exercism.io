(defpackage :strain
  (:use :cl)
  (:export :keep :discard))

(in-package :strain)

(defun keep (keep-p elements)
  "Returns a sublist of elements according to a given predicate."
  (reduce #'(lambda (selected element)
              (append selected (when (funcall keep-p element) (list element))))
          elements
          :initial-value '()))

(defun discard (discard-p elements)
  "Returns a sublist of elements not matching a given predicate."
  ;(keep #'(lambda (element) (not (funcall discard-p element))) elements))
  (keep (complement discard-p) elements))
