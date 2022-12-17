(defpackage :diamond
  (:use :cl)
  (:export :rows))

(in-package :diamond)

(defconstant +A+ (char-code #\A))

(defun blanks (n) (format nil "~v@{~A~:*~}" n #\Space))

(defun rows (letter)
  (loop :with n = (1+ (- (char-code letter) +A+))
        :for i :from 0 :below n
        :for half-row = (blanks n)
        :for row-letter = (code-char (+ i +A+))
        :do (setf (elt half-row i) row-letter)
        :collect (concatenate 'string (reverse (subseq half-row 1)) half-row)
                 :into top-half
        :finally (return (append top-half (subseq (reverse top-half) 1)))))
