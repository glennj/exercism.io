(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

(defconstant +minutes-per-layer+ 19 "time to prepare each layer of the lasagne")
(defconstant +cooking-time+ 337 "default cooking time for a lasagne")

(defun preparation-time (&rest layers)
  (* (length layers) +minutes-per-layer+))

(defun cooking-time-changes (label)
  (case label
    (:very-short -200)
    (:shorter    -100)
    (:longer      100)
    (:very-long   200)
    (otherwise      0)))

(defun remaining-minutes-in-oven (&optional (cooking-amount :normal cooking-amount-p))
  (if (and cooking-amount-p (not cooking-amount))
    0  ; argument specified and was falsy
    (+ +cooking-time+ (cooking-time-changes cooking-amount))))

(defun split-leftovers (&key (weight nil weight-given-p) (human 10) (alien 10))
  (if (not weight-given-p)
    :just-split-it
    (if weight 
      (- weight (+ human alien)) 
      :looks-like-someone-was-hungry)))
