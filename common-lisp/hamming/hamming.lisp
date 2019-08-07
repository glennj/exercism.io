(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands."
  (when (= (length dna1) (length dna2)) 
    ;(loop for i below (length dna1)
    ;      count (char/= (char dna1 i) (char dna2 i)))))
    (loop for i across dna1
          for j across dna2
          count (char/= i j))))
