(defpackage :nth-prime
  (:use :cl)
  (:export :find-prime))

(in-package :nth-prime)

(defun find-prime (number &aux (primes '(2 3)))
  (labels ((primep (n)
                   (loop with limit = (floor (sqrt n))
                         for p in primes
                         while (<= p limit)
                         when (zerop (mod n p)) do (return nil)
                         finally (return t)))
           (add-next-prime ()
                           (loop with current = (car (last primes))
                                 for candidate = (+ 2 current) then (+ 2 candidate)
                                 until (primep candidate)
                                 finally (setf primes (append primes (list candidate))))))
    (when (plusp number)
      (loop for len = (length primes) then (1+ len)
            while (< len number)
            do (add-next-prime)
            finally (return (nth (1- number) primes))))))
