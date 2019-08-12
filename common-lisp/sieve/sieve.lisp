(defpackage #:sieve
  (:use #:cl)
  (:export #:primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package #:sieve)

(defun primes-to (limit)
  (let ((sieve (make-list (1+ limit) :initial-element T)))

    ;; set all elements of the list to NIL where the index is non-prime
    (loop
      for i from 2 upto (floor (sqrt limit))
      when (nth i sieve)
        do (loop
             with step = (* i (if (= i 2) 1 2))
             for j from (* i i) upto limit by step
             do (setf (nth j sieve) NIL)))

    ;; collect all the indices of the sieve where the element is T
    (loop
      for i from 2 upto limit
      when (nth i sieve) collect i)))
