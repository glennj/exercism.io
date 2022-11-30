(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

(defun categorize-number (lists number) 
  (if (evenp number)
      (cons (car lists) (cons number (cdr lists)))
      (cons (cons number (car lists)) (cdr lists))))

(defun partition-numbers (list)
  (reduce 'categorize-number list :initial-value '(nil)))
  