(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)

(defun factors (n &optional (fact (floor (sqrt n))) (factors '()))
  (if (= 1 fact)
    (remove n (remove-duplicates (append factors (list 1 n))))
    (multiple-value-bind (q r) (floor n fact)
      (factors n
               (1- fact)
               (append factors (when (zerop r) (list fact q)))))))

(defun aliquot-sum (n) (reduce '+ (factors n)))

(defun classify (n)
  (when (> n 0)
    (let ((sum (aliquot-sum n)))
      (cond ((< sum n) "deficient")
            ((> sum n) "abundant")
            (t         "perfect")))))
