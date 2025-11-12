(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "resistor-color")
  (load "resistor-color-duo"))

(defpackage :resistor-color-trio
  (:use :cl)
  (:export :label))

(in-package :resistor-color-trio)

(defun label (colors)
  (let* ((bands12    (resistor-color-duo:value (subseq colors 0 2)))
         (band3      (resistor-color:color-code (third colors)))
         (resistance (* bands12 (expt 10 band3))))

    (loop with idx = 0
          while (and (plusp resistance) (zerop (mod resistance 1000)))
          do (incf idx)
          do (setf resistance (/ resistance 1000))
          finally (return
                    (format nil
                            "~a ~aohms"
                            resistance
                            (nth idx '("" "kilo" "mega" "giga")))))))
