(defpackage :square-root
  (:use :cl)
  (:export :square-root))

(in-package :square-root)

; Using the Binary numeral system (base 2) from Wikipedia
; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
;
(defun square-root (radicand)
  (loop with n = radicand
        with b = (expt 4 (floor (log n 4))) ; greatest power of 4 <= n
        with x = 0
        while (plusp b)
        if (>= n (+ x b))
          do (setf n (- n x b))
          and do (setf x (+ (/ x 2) b))
        else
          do (setf x (/ x 2))
        end
        do (setf b (floor b 4))   ; integer division
        finally (return x)))
