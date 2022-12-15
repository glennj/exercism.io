(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defconstant +gigasecond+ 1000000000)

(defun from (y mo d h m s)
  (let* ((tz 0) ;; GMT time zone
         (now (encode-universal-time s m h d mo y tz))
         (future (+ now +gigasecond+)))
    ;; decode-universal-time:
    ;; "Nine values are returned:
    ;;    second, minute, hour,
    ;;    date, month, year,
    ;;    day-of-week, daylight-saving-time-p, time-zone"
    (let ((future-time (multiple-value-list (decode-universal-time future tz))))
      (list (sixth future-time)
            (fifth future-time)
            (fourth future-time)
            (third future-time)
            (second future-time)
            (first future-time)))))
