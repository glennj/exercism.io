;;;; using algorithm from ILoveMuffins's python solution:
;;;; https://exercism.io/tracks/python/exercises/palindrome-products/solutions/d68cab86cad94d4d821f26da44bb0722

(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun palindromep (n &aux (s (format nil "~a" n)))
  (string= s (reverse s)))

(defun factorize (n f1 step cmp in-range)
  (loop :for f = f1 :then (+ f step)
        :while (funcall cmp (* f f) n)
        :for f2 = (floor n f)
        :when (and (= n (* f f2)) (funcall in-range f2))
          :collect (sort (list f f2) #'<)))

(defun find-solution (&key product-start product-end step factor-start cmp in-range)
  (loop :with factors
        :for product = product-start :then (+ product step)
        :while (funcall cmp product product-end)
        :when (palindromep product)
          :do (setf factors (factorize product factor-start step cmp in-range))
          :and :when (plusp (length factors))
                 :do (return (values product factors))))

(defun smallest (min-factor max-factor)
  (when (<= min-factor max-factor)
    (find-solution :product-start (* min-factor min-factor)
                   :product-end (* max-factor max-factor)
                   :step 1
                   :factor-start min-factor
                   :cmp (lambda (a b) (<= a b))
                   :in-range (lambda (n) (<= min-factor n max-factor)))))

(defun largest (min-factor max-factor)
  (when (<= min-factor max-factor)
    (find-solution :product-start (* max-factor max-factor)
                   :product-end (* min-factor min-factor)
                   :step -1
                   :factor-start max-factor
                   :cmp (lambda (a b) (>= a b))
                   :in-range (lambda (n) (<= min-factor n max-factor)))))
