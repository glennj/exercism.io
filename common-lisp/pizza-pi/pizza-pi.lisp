(defpackage :pizza-pi
  (:use :cl)
  (:export :dough-calculator :pizzas-per-cube
           :size-from-sauce :fair-share-p))

(in-package :pizza-pi)

;; Task 1
(defconstant +base-dough+ 200 "amount of dough in grams for the 'base' (i.e. non-crust) of the pizza")
(defconstant +dough-per-length-of-crust+ 45 "amount of dough in grams per length of crust")
(defconstant +len+ 20 "size in cm of a 'length of crust'")

(defun perimeter (diameter) 
	"the perimeter of a circle given the diameter"
	(* pi diameter))

(defun lengths-of-crust (diameter)
  (/ (perimeter diameter) +len+))

(defun crust-dough (diameter)
  (* (lengths-of-crust diameter) +dough-per-length-of-crust+))

(defun dough-calculator (pizzas diameter)
  (round (* pizzas (+ (crust-dough diameter) +base-dough+))))

;; Task 2
(defconstant +sauce-unit+ (/ 3 10) "amount of sauce in mL per cm^2 of pizza")

(defun diameter-from-area (area)
  "given an area of a circle, return the diameter"
  ; a = pi r^2 => r = sqrt(a / pi) => d = 2 * sqrt(a/pi)
  (* 2 (sqrt (/ area pi))))

(defun size-from-sauce (sauce)
  (diameter-from-area (/ sauce +sauce-unit+)))

;; Task 3
(defun cubed (n) (* n n n))
(defun squared (n) (* n n))

(defun pizzas-per-cube (cube-size diameter)
  ; n = (2 * (l^3))/(3 * pi * (d^2))
  (floor (/ (* 2 (cubed cube-size))
            (* 3 pi (squared diameter)))))

;; Task 4
(defconstant +slices-per-pizza+ 8)

(defun fair-share-p (pizzas friends)
  "true if the number of slices can be evenly divided among the friends"
  (zerop (mod (* pizzas +slices-per-pizza+)
              friends)))
