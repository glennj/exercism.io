(defpackage :gigasecond-anniversary
  (:use :cl)
  (:export :from))
(in-package :gigasecond-anniversary)

(defconstant +Gigasecond+ 1000000000 "one billion seconds")
(defconstant +utc+ 0 "zero time zone offset from UTC")

(defun from (year month day hour minute second)
  (multiple-value-bind 
    (s m h dd mm yy dow dst tz) 
    (decode-universal-time (+ +Gigasecond+
                              (encode-universal-time second minute hour day month year)))
    (list yy mm dd h m s)))