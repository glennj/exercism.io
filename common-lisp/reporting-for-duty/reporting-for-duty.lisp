(defpackage :reporting-for-duty
  (:use :cl)
  (:export :format-quarter-value :format-two-quarters
           :format-two-quarters-for-reading))

(in-package :reporting-for-duty)

(defun format-quarter-value (quarter value)
  (format nil "The value ~a quarter: ~a" quarter value))

(defun format-two-quarters (stream q1 v1 q2 v2)
  (format stream "~%~a~%~a~%" 
          (format-quarter-value q1 v1)
          (format-quarter-value q2 v2)))

(defun format-two-quarters-for-reading (stream q1 v1 q2 v2)
  (format stream "~s"
          (list (format-quarter-value q1 v1)
                (format-quarter-value q2 v2))))
