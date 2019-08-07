(in-package #:cl-user)
(defpackage #:all-your-base
  (:use #:common-lisp)
  (:export #:rebase))

(in-package #:all-your-base)

(defun rebase (list-digits in-base out-base)
  (if (and
        (>= in-base 2)
        (>= out-base 2)
        (every #'(lambda (d) (<= 0 d (1- in-base))) list-digits))
    (let ((sum 0))
      ;; calculate the decimal value of the incoming digits
      (dolist (digit list-digits)
        (setf sum (+ (* sum in-base) digit)))
      ;; and convert that to outgoing list of digits
      (loop
        until (zerop sum)
        collecting (mod sum out-base) into digits
        do (setf sum (floor sum out-base))
        finally (return (or (reverse digits) (list 0)))))))
