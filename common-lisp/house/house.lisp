(defpackage :house
  (:use :cl)
  (:export :recite))

(in-package :house)

(defconstant +data+ '("house that Jack built."
                      "malt that lay in"
                      "rat that ate"
                      "cat that killed"
                      "dog that worried"
                      "cow with the crumpled horn that tossed"
                      "maiden all forlorn that milked"
                      "man all tattered and torn that kissed"
                      "priest all shaven and shorn that married"
                      "rooster that crowed in the morn that woke"
                      "farmer sowing his corn that kept"
                      "horse and the hound and the horn that belonged to"))

(defun verse (n)
  (format nil "This is~{ the ~a~}" (reverse (subseq +data+ 0 n))))

(defun recite (start-verse end-verse)
  (let* ((range  (loop for i from start-verse to end-verse collect i))
         (verses (map 'list 'verse range)))
    (format nil "~{~a~^~%~}" verses)))
