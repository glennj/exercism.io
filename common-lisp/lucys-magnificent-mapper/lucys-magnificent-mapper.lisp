(defpackage :lucys-magnificent-mapper
  (:use :cl)
  (:export :make-magnificent-maybe :only-the-best))

(in-package :lucys-magnificent-mapper)

(defun make-magnificent-maybe (function list)
  (mapcar function list))

(defun only-the-best (function list) 
  "from the list, remove the number 1 and those elements for which the function returns true"
  (remove-if function 
             (remove 1 list)))
