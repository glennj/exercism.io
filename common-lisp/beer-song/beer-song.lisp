(defpackage :beer-song
  (:use :cl)
  (:export :verse :sing))

(in-package :beer-song)

(defconstant +max-bottles+ 99 "for God's sake, make it stop")

(defun onep (n) (= 1 n))

(defun bottles (n)
  (let ((n (if (< n 0) +max-bottles+ n)))
    (format nil
            "~a bottle~a of beer"
            (if (zerop n) "no more" n)
            (if (onep n) "" "s"))))

(defun action (n) 
  (if (zerop n)
    "Go to the store and buy some more"
    (format nil
            "Take ~a down and pass it around"
            (if (onep n) "it" "one"))))


(defun verse (n)
  "Returns a string verse for a given number."
  (let ((b (bottles n))
        (where "on the wall"))
    (format nil
            "~a ~a, ~a.~%~a, ~a ~a.~%"
            (string-capitalize b :end 1)
            where
            b
            (action n)
            (bottles (1- n))
            where)))

(defun sing-rec (str n end)
  (unless (< n end)
    (format str "~a~%" (verse n))
    (sing-rec str (1- n) end)))

(defun sing (start &optional (end 0))
  "Returns a string of verses for a given range of numbers."
  (with-output-to-string (str) (sing-rec str start end)))
