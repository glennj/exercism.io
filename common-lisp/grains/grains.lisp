(in-package #:cl-user)
(defpackage #:grains
  (:use #:cl)
  (:export :square :total))
(in-package #:grains)

(defun square (n) (expt 2 (1- n)))

(defun total () 
  ;(loop for i from 1 to 64 summing (square i)))
  (1- (expt 2 64)))
