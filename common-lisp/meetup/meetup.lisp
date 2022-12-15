(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

(defconstant +day-names+ 
  '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defun time-element (epoch idx) (nth-value idx (decode-universal-time epoch 0)))

(defun day-of-month (epoch) (time-element epoch 3))
(defun day-of-week  (epoch) (time-element epoch 6))

(defun day-of-week-name (epoch) (nth (day-of-week epoch) +day-names+))

(defun last-day-of-month (month year)
  (let ((next-month
         (if (= 12 month)
           (encode-universal-time 0 0 0 1 1 (1+ year) 0)
           (encode-universal-time 0 0 0 1 (1+ month) year 0))))
    (day-of-month (- next-month 86400))))

(defun days-of-month (month year)
  "Return a hash table mapping the day-of-week name to the
   list of its day numbers for that month/year."

  ;; CL-USER(8): (defvar m (days-of-month 12 2022))
  ;; CL-USER(9): (mapcar #'(lambda (dow) (gethash dow m)) +day-names+)
  ;;
  ;; ((5 12 19 26) (6 13 20 27) (7 14 21 28) (1 8 15 22 29) (2 9 16 23 30)
  ;;  (3 10 17 24 31) (4 11 18 25))

  (loop with day-name
        with epoch
        with month-days = (make-hash-table)
        for day from 1 upto (last-day-of-month month year)
        do
          (setf epoch (encode-universal-time 0 0 0 day month year 0))
          (setf day-name (day-of-week-name epoch))
          (setf (gethash day-name month-days)
                (append (gethash day-name month-days)
                        (list day)))
        finally (return month-days)))

(defun meetup (month year dow schedule)
  "Returns a date in the format (y m d) for a given meetup date."
  (let ((days (gethash dow (days-of-month month year))))
    (list year
          month
          (case schedule
            (:last   (car (last days)))
            (:teenth (find-if #'(lambda (d) (<= 13 d 19)) days))
            ((:first :second :third :fourth)
              (funcall (find-symbol (symbol-name schedule)) days))))))
