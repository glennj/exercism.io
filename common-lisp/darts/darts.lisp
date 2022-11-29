(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun hypot (x y) (sqrt (+ (* x x) (* y y))))

(defun score (x y)
  (let ((dist (hypot x y)))
    (cond ((<= dist  1) 10)
          ((<= dist  5)  5)
          ((<= dist 10)  1)
          (t             0))))
