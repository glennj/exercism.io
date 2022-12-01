(defpackage :proverb
  (:use :cl)
  (:export :recite))

(in-package :proverb)

(defun and-all (initial)
  (format nil "And all for the want of a ~a." initial))

(defun for-want (this that)
  (format nil "For want of a ~a the ~a was lost." this that))

(defun recite-rec (initial items &optional (proverb '()))
  (if (= 1 (length items))
      (cons (and-all initial) proverb)
      (recite-rec initial
                  (cdr items)
                  (cons (for-want (car items) (cadr items))
                        proverb))))

(defun recite (strings)
  (if (null strings)
      ""
      (format nil
              "~{~&~a~}"
              (reverse (recite-rec (car strings) strings)))))

