(defpackage :gigasecond-anniversary
  (:use :cl)
  (:export :from))
(in-package :gigasecond-anniversary)

(defconstant +Gigasecond+ 1000000000 "one billion seconds")
(defconstant +utc+ 0 "zero time zone offset from UTC")

(defun from (year month day hour minute seconds)
  (let* ((epoch (encode-universal-time seconds minute hour day month year +utc+))
         (future-epoch (+ epoch +Gigasecond+))
         (future (multiple-value-list (decode-universal-time future-epoch +utc+))))
    ;; return the future '(year month day hour minute seconds)
    (list (sixth future)
          (fifth future)
          (fourth future)
          (third future)
          (second future)
          (first future))))
