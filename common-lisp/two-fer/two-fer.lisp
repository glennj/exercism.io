(in-package #:cl-user)
(defpackage #:two-fer
  (:use #:cl)
  (:export #:twofer))
(in-package #:two-fer)

;; various methods of accomplishing this task:

(defun twofer-if (name)
  (format nil "One for ~a, one for me." 
    (if (null name) "you" name)))

(defun twofer-cond (name)
  (cond ((null name) (setq name "you")))
  (format nil "One for ~a, one for me." name))

(defun twofer-or (name)
  (format nil "One for ~a, one for me." 
    (or name "you")))

(defun twofer-format (name)
  (format nil "One for ~:[you~;~:*~a~], one for me." name))

(defun twofer (&optional (name "you"))
  ;(twofer-if name))
  (twofer-cond name))
  ;(twofer-or name))
  ;(twofer-format name))
