(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))
(in-package :pascals-triangle)

;; TODO memoize factorials.
(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (1- n)))))

(defun n-choose-k (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(defun rows (n)
  (when (> n 0)
    (loop for i upto (1- n)
          collect (loop for j upto i
                        collect (n-choose-k i j)))))
