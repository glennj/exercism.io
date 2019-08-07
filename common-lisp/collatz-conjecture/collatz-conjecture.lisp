(defpackage #:collatz-conjecture
  (:use #:common-lisp)
  (:export #:collatz))

(in-package #:collatz-conjecture)

(defun collatz (n &optional steps)
  (let ((steps (or steps 0)))
    (cond
      ((< n 1)   nil)
      ((= n 1)   steps)
      ((evenp n) (collatz (/ n 2)      (1+ steps)))
      (t         (collatz (1+ (* n 3)) (1+ steps))))))
