(defpackage :say
  (:use :cl)
  (:export :say))

(in-package :say)

(defconstant +ones+ #(
  "zero" "one" "two" "three" "four" "five" "six" "seven" "eight"
  "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
  "sixteen" "seventeen" "eighteen" "nineteen"))

(defconstant +tens+ #(
  nil nil "twenty" "thirty" "forty" "fifty"
  "sixty" "seventy" "eighty" "ninety"))

(defun join (words &key (separator " "))
  (let ((fmt (format nil "~~{~~a~~^~a~~}" separator)))
    (format nil fmt words)))

;; "forward declaration" to prevent warnings:
;; > caught STYLE-WARNING: undefined function: SAY:SAY
;; Here's the cheeky builtin answer:
(defun say (number) (format nil "~r" number))


(defun say-small (number)
  "Express numbers less than 100 in words"
  (if (< number 20)
      (elt +ones+ number)
      (multiple-value-bind (quo rem) (floor number 10)
        (let ((words '()))
          (unless (zerop rem) (push (elt +ones+ rem) words))
          (push (elt +tens+ quo) words)
          (join words :separator "-")))))

(defun say-compound (number divisor unit)
  "Express large numbers in words"
  (multiple-value-bind (quo rem) (floor number divisor)
    (let ((parts '()))
      (unless (zerop rem) (push (say rem) parts))
      (push unit parts)
      (push (say quo) parts)
      (join parts))))

(defun say (number)
  "Express a number in words"
  (cond
    ((< number 0) nil)
    ((< number 100) (say-small number))
    ((< number 1000) (say-compound number 100 "hundred"))
    ((< number 1000000) (say-compound number 1000 "thousand"))
    ((< number 1000000000) (say-compound number 1000000 "million"))
    ((< number 1000000000000) (say-compound number 1000000000 "billion"))
    (t nil)))
