(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)


;; Recursive

(defun largest-product (digits span &aux (len (length digits)))
  (labels ((helper (idx maximum)
             (if (> idx (- len span))
                 maximum
                 (let* ((seq (subseq digits idx (+ idx span)))
                        (product (apply #'* (map 'list #'digit-char-p seq))))
                   (helper (1+ idx) (max maximum product))))))
    (when (and (>= len span 0)
               (every #'digit-char-p digits))
      (helper 0 -1))))


;; Using LOOP

(defun product-of (digit-string)
  "The product of the digits in a string.  Returns nil if there is a non-digit in the string."
  (handler-case
    (reduce #'* (map 'list #'digit-char-p digit-string) :initial-value 1)
    ;; non-digit leads to multiplying a number by nil
    (type-error () nil)))

(defun largest-product-loop (digits span &aux (len (length digits)))
	(when (>= len span 0)
		(loop with max-product = -1
					with product
					for i from 0 upto (- len span)
					do (setf product (product-of (subseq digits i (+ i span))))
					never (null product)
					do (setf max-product (max max-product product))
					finally (return max-product))))




;; delightful minimalist solution from the community
;; https://exercism.org/tracks/common-lisp/exercises/largest-series-product/solutions/leetwinski
(defun largest-product-leetwinski (digits span &aux (len (length digits)))
  (when (and (<= 0 span len)
             (every #'digit-char-p digits))
    (loop for x upto (- len span)
          for chunk = (subseq digits x (+ x span))
          for prod = (apply #'* (map 'list #'digit-char-p chunk))
          maximize prod)))
