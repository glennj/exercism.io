(defpackage :binary-search
  (:use :cl)
  (:export :binary-find :value-error))

(in-package :binary-search)

(defun average (a b) (floor (+ a b) 2))

(defun binary-find (arr el &optional (left 0) (right (1- (length arr))))
  (when (<= left right)
    (let* ((mid (average left right))
           (item (elt arr mid)))
      (cond
        ((= el item) mid)
        ((< el item) (binary-find arr el left (1- mid)))
        (t           (binary-find arr el (1+ mid) right))))))
