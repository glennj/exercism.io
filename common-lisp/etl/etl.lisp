(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data &optional (transformed (make-hash-table)))
  "Transforms hash values into keys with their keys as their values."
  (let* ((transformed (make-hash-table))
         (transformer #'(lambda (score letters)
                          (dolist (letter letters)
                            (setf (gethash (char-downcase letter) transformed) score)))))
    (maphash transformer data)
    transformed))
