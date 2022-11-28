(defpackage :socks-and-sexprs
  (:use :cl)
  (:export :lennys-favorite-food :lennys-secret-keyword
           :is-an-atom-p :is-a-cons-p :first-thing :rest-of-it))

(in-package :socks-and-sexprs)

;; Evaluates to some symbol (not a keyword)
(defun lennys-favorite-food ()
  'foo
)

;; Evaluates to some keyword
(defun lennys-secret-keyword ()
  :bar
)

;; Evaluates to T if THING is an atom, NIL otherwise
(defun is-an-atom-p (thing) (atom thing))

;; Evaluates to T if THING is a cons, NIL otherwise
(defun is-a-cons-p (thing) (consp thing))

;; Evaluates to the first part of CONS
(defun first-thing (cons) (car cons))

;; Evaluates to the 'rest' of the CONS
(defun rest-of-it (cons) (cdr cons))
