(in-package #:cl-user)
(defpackage #:space-age
  (:use #:common-lisp))
(in-package #:space-age)

;; notes
;; * string to be intern'ed must be uppercased
;; * need a `let` in the first do block so the loop variable
;;   is not shared between all the functions (every planet has
;;   Neptune's orbit)

(loop
  with seconds-per-earth-year = 31557600
  for planet    in '("mercury" "venus" "earth" "mars" "jupiter" "saturn" "uranus" "neptune")
  for rel-orbit in '(0.2408467 0.61519726 1.0 1.8808158 11.862615 29.447498 84.016846 164.79132)
  for func-name = (intern (string-upcase (concatenate 'string "on-" planet)))
  do 
    (let ((seconds-per-orbit (* rel-orbit seconds-per-earth-year)))
      (setf (fdefinition func-name) 
            #'(lambda (age) (/ age seconds-per-orbit))))
  do (export func-name))

