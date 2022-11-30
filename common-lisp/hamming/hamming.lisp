(defpackage #:hamming
  (:use #:cl)
  (:export #:distance))

(in-package #:hamming)

;; an iterative approach
(defun distance-loop (dna1 dna2)
  ;(loop for i below (length dna1)
  ;      count (char/= (char dna1 i) (char dna2 i)))))
  (loop for i across dna1
        for j across dna2
        count (char/= i j)))

;; a recursive approach
(defun distance-counter (n a b)
  (if (null a)
    n
    (distance-counter
      (+ n (if (char= (car a) (car b)) 0 1))
      (cdr a)
      (cdr b))))

(defun distance-recursive (dna1 dna2)
  (distance-counter 0
           (coerce dna1 'list)
           (coerce dna2 'list)))

(defun distance (dna1 dna2)
  "Number of positional differences in two equal length dna strands.
   Returns NIL if strings lengths are not equal."

  (when (= (length dna1) (length dna2))
    ;(distance-loop dna1 dna2)
    (distance-recursive dna1 dna2)))
