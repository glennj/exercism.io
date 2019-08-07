(defpackage #:raindrops
  (:use #:common-lisp)
  (:export #:convert))

(in-package #:raindrops)

(defun convert (num)
  (let ((rain (concatenate 'string ""  
                (and (zerop (mod num 3)) "Pling")
                (and (zerop (mod num 5)) "Plang")
                (and (zerop (mod num 7)) "Plong"))))

  (if (string= "" rain)
      (format nil "~a" num)
      rain)))
