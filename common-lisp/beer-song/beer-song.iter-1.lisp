(defpackage :beer-song
  (:use :cl)
  (:export :verse :sing))
(in-package :beer-song)
(defconstant +max-bottles+ 99 "for God's sake, make it stop")
;; ref https://dnaeon.github.io/generating-sequences-in-common-lisp/
(defun range (start stop &key (step 1))
  "Recursive RANGE function"
  (when (and (> start stop) (plusp step))
    (error (format nil "Invalid positive step: start=~a, stop=~a, step=~a" start stop step)))
  (when (and (< start stop) (minusp step))
    (error (format nil "Invalid negative step: start=~a, stop=~a, step=~a" start stop step)))
  (when (zerop step)
    (error (format nil "Invalid zero step: start=~a, stop=~a, step=~a" start stop step)))
  (labels ((recur (i acc)
	     (cond
	       ((and (minusp step) (<= i stop)) (nreverse acc))
	       ((and (plusp step) (>= i stop)) (nreverse acc))
	       (t (recur (+ i step) (push i acc))))))
    (recur start nil)))
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
(defun sing (start &optional (end 0))
  "Returns a string of verses for a given range of numbers."
  (reduce
    (lambda (verses n) (format nil "~a~a~%" verses (verse n)))
    (range start (1- end) :step -1)
    :initial-value ""))
