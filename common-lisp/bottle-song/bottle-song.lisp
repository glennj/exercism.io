(defpackage :bottle-song
  (:use :common-lisp)
  (:export :recite))

(in-package :bottle-song)

(defconstant +nums+ '("no" "one" "two" "three" "four" "five"
                         "six" "seven" "eight" "nine" "ten"))

(defun recite (start-bottles take-down)
  (labels (
      (bottles (n) (format nil "~[no~:;~:*~r~] green bottle~p hanging on the wall" n n))
      (line1 (n) (format nil "~@(~a~)," (bottles n)))
      (line3 () "And if one green bottle should accidentally fall,")
      (line4 (n) (format nil "There'll be ~a." (bottles (1- n))))
      (verse (n) (list "" (line1 n) (line1 n) (line3) (line4 n)))
      (helper (n remaining song)
        (if (zerop remaining)
          song
          (helper (1- n) (1- remaining) (append song (verse n))))))

    (cdr (helper start-bottles take-down '()))))
