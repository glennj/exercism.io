(defpackage :pangram
  (:use :cl)
  (:export :pangramp))

(in-package :pangram)

(defun pangramp (sentence)
  (loop with letters = "abcdefghijklmnopqrstuvwxyz"
        for c across sentence
        when (alpha-char-p c)
          do (setf letters (remove (char-downcase c) letters))
        when (zerop (length letters))
          do (return t)))
