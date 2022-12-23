;; This is a translation of my Tcl solution which is a translation of the Java reference solution
;; https://exercism.org/tracks/tcl/exercises/book-store/solutions/glennj

(defpackage :book-store
  (:use :cl)
  (:export :calculate-price))

(in-package :book-store)

(defun group-cost ()
  (loop :with costs = (make-array 6 :initial-element 0)
        :for n :from 1 :upto 5
        :for d :in '(0 5 10 20 25)
        :do (setf (elt costs n) (/ (* n 800 (- 100 d)) 100))
        :finally (return costs)))

(defvar *group-costs* (group-cost))

;; this sorts a basket of books (like '(1 2 2 3 3 4 4 4 4))
;; by group size descending, stably => '(4 4 4 4 2 2 3 3 1)
(defun sort-by-group-size (basket)
  (let ((counts (make-hash-table))
        (seen '()))
    (loop :for book :in basket
          :unless (gethash book counts)
            :do (setf (gethash book counts) 0)
            :and :do (setf seen (append seen (list book)))
          :do (incf (gethash book counts)))
    (let* ((groups (loop :for book :in seen
                         :collect (list book (gethash book counts))))
           (sorted (stable-sort groups #'> :key #'second)))
      (flet ((repeat (x n) (loop :repeat n :collect x)))
        (apply #'append
               (mapcar #'(lambda (pair) (apply #'repeat pair))
                       sorted))))))

;; Find the unique elements of a list, while preserving the order
(defun uniq (l)
  (if (zerop (length l))
      '()
      (if (member (car l) (cdr l))
          (uniq (cdr l))
          (cons (car l) (uniq (cdr l))))))

;; Given a list of items to remove, remove only the first of each
;; (remove-first '(1 2 3) '(3 3 3 2 2 1)) ; ==> '(3 3 2)
(defun remove-first (list-to-remove items)
  (loop :for item :in items
        :if (member item list-to-remove)
          :do (setf list-to-remove (remove item list-to-remove))
        :else
          :collect item))

;; this recursively calculates the minimum cost
(defun calculate-basket-cost (books price)
  (if (zerop (length books))
      price
      (let ((uniq-books (uniq books)))
        (loop :for i :from 1 :upto (length uniq-books)
              :for new-group-books = (subseq uniq-books 0 i)
              :for remaining-books = (remove-first new-group-books books)
              :minimizing (calculate-basket-cost remaining-books
                                                 (+ price (elt *group-costs* i)))))))

(defun calculate-price (basket)
  (let ((ordered (sort-by-group-size basket)))
    (calculate-basket-cost ordered 0)))
