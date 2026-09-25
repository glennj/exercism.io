(defpackage :line-up
  (:use :cl)
  (:shadow format)
  (:export :format))

(in-package :line-up)

(defun format (name number)
  (cl:format nil "~A, you are the ~D~A customer we serve today. Thank you!"
             name number (ordinal number)))

(defun ordinal (number)
  (case (mod number 100)
    ((1 21 31 41 51 61 71 81 91) "st")
    ((2 22 32 42 52 62 72 82 92) "nd")
    ((3 23 33 43 53 63 73 83 93) "rd")
    (t "th")))
