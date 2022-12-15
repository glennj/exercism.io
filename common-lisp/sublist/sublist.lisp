(defpackage #:sublist
  (:use #:common-lisp)
  (:export #:sublist))

(in-package #:sublist)

(defun sublist-p (A B)
  "Is A contained within B?"
  (loop with lenA = (length A)
        with lenB = (length B)
        for i from 0 upto (- lenB lenA)
        for sliceB = (subseq B i (+ i lenA))
        when (equal A sliceB) return t))

(defun sublist (list1 list2)
  "what is list1 of list2 (sublist, superlist, equal or unequal)"
  (cond
    ((equal     list1 list2) :equal)
    ((sublist-p list1 list2) :sublist)
    ((sublist-p list2 list1) :superlist)
    (t                       :unequal)))
