(in-package #:cl-user)
(defpackage #:triangle
  (:use #:cl)
  (:export #:triangle))

(in-package #:triangle)

;; take 2
;; I keep forgetting that comparison functions can take > 2 args
(defun triangle (&rest sides)
  (destructuring-bind (a b c) (sort sides #'<)
    (cond
      ((or (<= a 0) (<= (+ a b) c)) :illogical)
      ((= a b c)  :equilateral)
      ((/= a b c) :scalene)
      (t          :isosceles))))

;; first take
; (defun triangle (a b c)
;   (let ((sides (sort (list a b c) #'<)))
;     (cond
;       ;; a side of a triangle must be a positive number
;       ((<= (nth 0 sides) 0) :illogical)
;       ;; the sum of the shorter sides cannot exceed the longest side
;       ((<= (+ (nth 0 sides) (nth 1 sides)) (nth 2 sides)) :illogical)
;       ;; otherwise, now many uniq lengths do we have?
;       (t (let ((uniq '()))
;            (pushnew a uniq)
;            (pushnew b uniq)
;            (pushnew c uniq)
;            (case (length uniq)
;              (1 :equilateral)
;              (2 :isosceles)
;              (3 :scalene)))))))
