(defpackage :allergies
  (:use :cl)
  (:shadow :list)
  (:export :allergic-to-p :list))

(in-package :allergies)

(defconstant +allergens+
             '("eggs" "peanuts" "shellfish" "strawberries"
               "tomatoes" "chocolate" "pollen" "cats"))

(defun shift-left (num i) (ash num (- i)))

(defun keep-if (test seq)
  (remove-if (lambda (elem) (not (funcall test elem))) seq))


(defun allergic-to-p (score allergen)
  "Returns true if given allergy score includes given allergen."
  (let ((idx (position allergen +allergens+ :test #'string=)))
    (when idx
      (= 1 (logand (shift-left score idx) 1)))))

(defun list (score)
  "Returns a list of allergens for a given allergy score."
  (keep-if (lambda (allergen) (allergic-to-p score allergen)) +allergens+))
