(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))
(in-package :crypto-square)

(defun encipher (plaintext)
  (let* ((normalized (string-downcase (remove-if-not #'alphanumericp plaintext)))
         (segment-length (ceiling (sqrt (length normalized))))
         (padded (format nil "~~~aa" segment-length))
         (rows (loop :while (< segment-length (length normalized))
                     :collect (subseq normalized 0 segment-length) :into segments
                     :do (setf normalized (subseq normalized segment-length))
                     :finally (return (append segments
                                              (list (format nil padded normalized))))))
         (transposed (loop :for i :below segment-length
                           :for column = (mapcar #'(lambda (chk) (elt chk i)) rows)
                           :collect (concatenate 'string column))))
    (format nil "~{~a~^ ~}" transposed)))
