(defpackage :armstrong-numbers
  (:use :cl)
  (:export :armstrong-number-p))
(in-package :armstrong-numbers)

(defun digits (num &optional (digits '()))
  "Return the decimal digits of an integer as a list"
  (if (zerop num)
      digits
      (multiple-value-bind (quo rem) (floor num 10)
        (digits quo (push rem digits)))))

(defun armstrong-number-p (number)
  "Test if a number is the sum of its own digits each raised to the power of the number of digits."
  (let* ((digits (digits number))
         (wid (length digits))
         (sum (reduce #'+ 
                      (mapcar (lambda (d) (expt d wid)) 
                              digits))))
    (= sum number)))
