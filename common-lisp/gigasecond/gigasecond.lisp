(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun from (y mo d h m s)
  (let* ((tz 0) ;; GMT time zone
         (now (encode-universal-time s m h d mo y tz))
         (future (+ now 1000000000)))
    ;; decode-universal-time:
    ;; "Nine values are returned: second, minute, hour,
    ;;  date, month, year, day-of-week,
    ;;  daylight-saving-time-p, and time-zone."
    (multiple-value-bind 
      (fs fm fh fd fmo fy fdoy fdst ftz)
      (decode-universal-time future tz)
      (list fy fmo fd fh fm fs))))
