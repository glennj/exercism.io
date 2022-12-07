(load "lib/math-utils")
(defpackage :perfect-numbers
  (:use :cl
        :math-utils)
  (:export :classify))

(in-package :perfect-numbers)

(defun aliquot-sum (n) (reduce '+ (math-utils:factors n)))

(defun classify (n)
  (when (> n 0)
    (let ((sum (aliquot-sum n)))
      (cond ((< sum n) "deficient")
            ((> sum n) "abundant")
            (t         "perfect")))))
