(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "resistor-color"))

(defpackage :resistor-color-duo
  (:use :cl)
  (:export :value))

(in-package :resistor-color-duo)

(defun value (colors)
  (+ (* 10 (resistor-color:color-code (first colors)))
     (resistor-color:color-code (second colors))))
