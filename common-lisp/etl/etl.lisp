(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
  "Transforms hash values into keys with their keys as their values."
  (let ((transformed (make-hash-table)))
    (flet ((transformer (score letters)
             (dolist (letter letters)
               (setf (gethash (char-downcase letter) transformed) score))))
      (maphash #'transformer data)
      transformed)))
