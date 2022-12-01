(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)

(defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (loop for c across str
        collect (dna-to-rna c) into rna
        finally (return (coerce rna 'string))))

(defun dna-to-rna (c)
  (case (char-upcase c)
    (#\G #\C) 
    (#\C #\G) 
    (#\T #\A) 
    (#\A #\U) 
    (t (error "Invalid nucleotide '~a'" c))))
