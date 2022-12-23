(defpackage :diamond
  (:use :cl)
  (:export :rows))

(in-package :diamond)

(defun rows (letter &aux (n (1+ (- (char-code letter) (char-code #\A)))))
  (loop :for i :from 0 :below n
        :for half-row = (format nil "~v@{~A~:*~}" n #\Space)
        :do (setf (elt half-row i) (code-char (+ i (char-code #\A))))
        :collect (concatenate 'string (reverse (subseq half-row 1)) half-row) :into top-half
        :finally (return (append top-half (subseq (reverse top-half) 1)))))
