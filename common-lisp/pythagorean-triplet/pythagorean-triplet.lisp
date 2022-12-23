(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun pythagorean-p (a b c)
  (= (+ (* a a) (* b b))
     (* c c)))

;; the smallest side `b` such that a <= b is where the sides
;; adjacent to the right angle are equal. 
;; Then, c² = b² + b² ==> c = √2 b
;; and   sum = b + b + c = b × (2 + √2) ==> b = sum ÷ (2 + √2)
(defun minimum-triangle-sides (sum)
	(let* ((b (floor (/ sum (+ 2 (sqrt 2)))))
				 (c (floor (* (sqrt 2) b))))
    (values b c)))

;; A brute force solution. Will be slow for large sums.
(defun triplets-with-sum (sum)
  (multiple-value-bind (b-min c-min) (minimum-triangle-sides sum)
    ;; magic numbers 3 and 4 are taken from the smallest pythagorean triplet
    ;; to save a few iterations
    (loop :for c :from (- sum 3 4) :downto c-min
          :collect (loop :for b :from (1- c) :downto b-min
                         :for a = (- sum c b)
                         :when (and (< 2 a b) (pythagorean-p a b c))
                           :collect (list a b c))
            :into triplets
          :finally (return (apply #'append triplets)))))
