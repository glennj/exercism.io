(in-package #:cl-user)
(defpackage #:triangle
  (:use #:cl)
  (:export #:triangle-type-p))

(in-package #:triangle)

(defun triangle-type-p (triangle-type &rest sides)
  (labels ((valid-triangle-p (a b c) (and (> a 0)
                                          (> (+ a b) c)))
           (equilateral-p  (a b c) (= a b c))
           (scalene-p      (a b c) (/= a b c))
           (isosceles-p    (a b c) (not (scalene-p a b c))))

    (destructuring-bind (a b c) (sort sides #'<)
      (when (valid-triangle-p a b c)
        (case triangle-type
          (:equilateral (equilateral-p  a b c))
          (:scalene     (scalene-p      a b c))
          (:isosceles   (isosceles-p    a b c)))))))
