(defpackage :resistor-color
  (:use :cl)
  (:export :color-code
           :colors))

(in-package :resistor-color)

(defun colors ()
  '("black" "brown" "red" "orange" "yellow" "green" "blue" "violet" "grey" "white"))

(defun color-code (color)
  (position color (colors) :test #'string=))
