(in-package #:cl-user)
(defpackage #:luhn
  (:use #:cl)
  (:export #:is-valid))

(in-package #:luhn)

(defun is-valid (input) 
  (loop
    for i from (1- (length input)) downto 0
    for c = (elt input i)
    if (digit-char-p c)
      count t into idx
      and sum (luhn-digit c idx) into sum
    else 
      if (char/= c #\Space)
        return nil
    finally 
      (return 
        (if (<= idx 1) 
          nil
          (zerop (mod sum 10))))))

(defun luhn-digit (c i)
  (let ((n (char-to-int c)))
    (if (oddp i) 
      n 
      (+ (floor (* n 2) 10) (mod (* n 2) 10)))))

(defun char-to-int (c)
  (- (char-code c) (char-code #\0)))
