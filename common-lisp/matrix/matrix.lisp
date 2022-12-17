(load "lib/string-utils")

(defpackage :matrix
  (:use :cl
        :string-utils)
  (:export :row
           :column))

(in-package :matrix)

(defun parse-input (input-matrix)
  (flet ((parse-line (line)
                     (mapcar #'parse-integer
                             (string-utils:split-string line :trim t))))
    (mapcar #'parse-line
            (string-utils:split-string input-matrix
                                       :separator #\Newline :trim t))))

(defun row (input-matrix index &aux (matrix (parse-input input-matrix)))
  (nth (1- index) matrix))
         
(defun column (input-matrix index &aux (matrix (parse-input input-matrix)))
  (mapcar #'(lambda (row) (nth (1- index) row))
          matrix))
