;;;; Some utility functions for solving Exercism exercises

(defpackage :string-utils
  (:use :cl)
  (:export :spaces
           :string-pop
           :string-repeat
           :char-digit-value))

(in-package :string-utils)

;; several exercism exercises want spaces inserted into a string at intervals
(defun spaces (str &key (result "") (size 5))
  "Add a space after each 5th character in a string"
  (flet ((add-word (str word) 
           (string-left-trim " " (concatenate 'string str " " word))))
    (if (<= (length str) size)
        (add-word result str)
        (spaces (subseq str size)
                :result (add-word result (subseq str 0 size))
                :size size))))

(defun string-pop (str)
  "Return the first character of a string (or NIL for an empty string)
  and the rest of the string."
  (if (string= "" str)
      (values nil "")
      (values (char str 0) (subseq str 1))))

(defun string-repeat (str-or-char num)
  (format nil "~v@{~A~:*~}" num str-or-char))

;; not really a "string" utility
(defun char-digit-value (c)
  "The numeric value of an ascii digit: (char-digit-value #\3) => 3"
  (- (char-int c) (char-int #\0)))
