(in-package #:cl-user)
(defpackage #:luhn
  (:use #:cl)
  (:export #:validp))

(in-package #:luhn)

(defvar *luhn-digits* (make-hash-table))
(setf (gethash t   *luhn-digits*) #(0 1 2 3 4 5 6 7 8 9))
(setf (gethash nil *luhn-digits*) #(0 2 4 6 8 1 3 5 7 9))

(defun luhn-digit (c i)
  (elt (gethash (oddp i) *luhn-digits*)
       (digit-char-p c)))

(defun validp (input) 
  (loop with idx = 0
        for c across (reverse input)
        if (digit-char-p c)
          do (incf idx)
          and sum (luhn-digit c idx) into sum
        else 
          if (char/= c #\Space)
            return nil
          end
        finally (return (when (> idx 1) (zerop (mod sum 10))))))
