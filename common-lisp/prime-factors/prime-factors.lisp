(defpackage :prime-factors
  (:use :cl)
  (:export :factors))

(in-package :prime-factors)

(defun factors (n &optional (fact 2) (factors '()))
  (if (> fact n)
    (append factors (when (> n 1) n))
    (multiple-value-bind (quo rem) (floor n fact)
      (if (zerop rem)
          (factors quo fact (append factors (list fact)))
          (factors n (1+ fact) factors)))))

;; an optimization:
;; replace `(1+ fact)` with `(+ fact (if (= fact 2) 1 2))`
;; that will skip over all the even numbers, reducing the iterations by half.
