(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)

(defun uniq (string) (remove-duplicates (string-downcase string)))
(defun sanitize (string) (remove #\Space (remove #\- string)))

(defun isogram-p (string)
  "Is string an Isogram?"
  (= (length (sanitize string)) (length (sanitize (uniq string)))))
