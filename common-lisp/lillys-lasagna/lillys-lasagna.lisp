(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven () "how many minutes the lasagne should be in the oven"
  337)

(defun remaining-minutes-in-oven (minutes-in-oven) "how many minutes remain"
  (- (expected-time-in-oven) minutes-in-oven))

(defconstant +minutes-per-layer+ 19 "time to prepare a single layer of a lasagne")

(defun preparation-time-in-minutes (number-of-layers)
   "how long to prepare a lasagne, given a number of layers"
  (* number-of-layers +minutes-per-layer+))

(defun elapsed-time-in-minutes (number-of-layers minutes-in-oven)
  "prep time plus time in oven"
  (+ (preparation-time-in-minutes number-of-layers) minutes-in-oven))
