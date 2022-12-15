(defpackage :leslies-lists
  (:use :cl)
  (:export :new-list
           :list-of-things
           :add-to-list
           :first-thing
           :second-thing
           :third-thing
           :twenty-third-thing
           :remove-first-item
           :on-the-list-p
           :list-append
           :just-how-long
           :part-of-list
           :list-reverse))

(in-package :leslies-lists)

;; task 1
;(defun new-list () '())
(defun new-list () (list))

#|
  Note `'(thing1 thing2 thing3)` won't work here:
  - the quoted list creates a list where all the elements are quoted
  - we need the args to be evaluated not quoted
|#
(defun list-of-things (thing1 thing2 thing3)
  (list thing1 thing2 thing3))

;; task 2
#|
  Could also use `(append (list item) list)`
  but that seems like excess work.
|#
(defun add-to-list (item list) (cons item list))

;; task 3
(defun first-thing (list) (car list))

(defun second-thing (list) (second list))

(defun third-thing (list) (third list))

(defun twenty-third-thing (list) (nth 22 list))

;; task 4
(defun remove-first-item (list) (cdr list))

;; task 5
(defun list-append (list1 list2) (append list1 list2))

;; task 6
;; the proper solution
(defun just-how-long (list) (length list))

;; experimenting with reimplementing `length`
#|
  Aye, here's the rub: how can you differentiate between an empty list
  and a list with one NIL element _without_ using `length` ?

    (length (list))      ; => 0
    (car (list))         ; => NIL
    (cdr (list))         ; => NIL

    (length (list NIL))  ; => 0
    (car (list NIL))     ; => NIL
    (cdr (list NIL))     ; => NIL

  Note: In Common Lisp all values are "true" except for () which is "false".
  There are two special constant symbols t and nil whose values are true and false respectively.

  Answer: `null` function -- http://www.lispworks.com/documentation/HyperSpec/Body/f_null.htm
|#

(defun just-how-long-counter (count list)
  (cond ((not (listp list))  nil)          ; this func is for counting the length of a list
        ((null list)         count)        ; empty list, stop recursion
        (t                   (just-how-long-counter (1+ count) (cdr list)))))

(defun just-how-long (list) (just-how-long-counter 0 list))
