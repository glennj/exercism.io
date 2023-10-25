(defpackage :nucleotide-count
  (:use :cl)
  (:export :dna-count :nucleotide-counts :invalid-nucleotide))

(in-package :nucleotide-count)

(defconstant +valid-nucleotides+ '(#\A #\C #\G #\T))

(define-condition invalid-nucleotide (error) ())

;; a debugging function
(defun print-hash (h) 
  (print h)
  (maphash (lambda (k v) (print (cons k v))) h))

(defun initialize-count-hash () 
  (let ((hash (make-hash-table)))
    (dolist (nucleotide +valid-nucleotides+)
      (setf (gethash nucleotide hash) 0))
    hash))

(defun validate-nucleotide (nucleotide)
  (unless (position nucleotide +valid-nucleotides+)
    (error 'invalid-nucleotide)))


(defun dna-count (nucleotide strand)
  "Returns a count of the given nucleotide appearing in a DNA strand."
  (validate-nucleotide nucleotide)
  (- (length strand)
     (length (remove nucleotide strand))))


;; A version re-using the dna-count variable.
;; I suspect this is pretty inefficient, passing over the strand several times
;; This one does not need to call validate-nucleotide: it's already iterating over the valid ones.
(defun nucleotide-counts--maphash (strand)
  (let ((counts (initialize-count-hash)))
    (maphash
      (lambda (key val)
        (setf (gethash key counts) (dna-count key strand)))
      counts)
    counts))

;; A version using an extended loop.
(defun nucleotide-counts--loop (strand)
  (loop 
    with counts = (initialize-count-hash)
    for nucleotide across strand
    do
      (validate-nucleotide nucleotide)
      (incf (gethash nucleotide counts))
    finally (return counts)))

;; and recursion
(defun nucleotide-counts--rec (strand &optional (counts (initialize-count-hash)))
  (if (string= "" strand)
    counts
    (let ((nucleotide (elt strand 0)))
      (validate-nucleotide nucleotide)
      (incf (gethash nucleotide counts))
      (nucleotide-counts--rec (subseq strand 1) counts))))


(defun nucleotide-counts (strand)
  "Returns a hash of nucleotides and their counts in a given DNA strand."
  ;(nucleotide-counts--maphash strand))
  ;(nucleotide-counts--loop strand))
  (nucleotide-counts--rec strand))
