;;;; Bucket class

(defpackage :bucket
  (:use :cl)
  ;; this package overrides :fill from the common-list package
  (:shadow :fill)
  ;; export the class, its accessors and methods
  (:export :bucket
           :name :size :amount
           :capacity :fullp :emptyp :fill :empty :pour))

(in-package :bucket)

(defgeneric capacity (b) (:documentation "the capacity of a bucket (how much can be poured into it)"))
(defgeneric fullp (b) (:documentation "predicate: is the bucket full?"))
(defgeneric emptyp (b) (:documentation "predicate: is the bucket empty?"))
(defgeneric fill (b) (:documentation "fill the bucket"))
(defgeneric empty (b) (:documentation "empty the bucket"))
(defgeneric pour-into (&key from into) (:documentation "pour from bucket into bucket"))

(defclass bucket ()
  ((name
     :reader name
     :type string
     :initform "aBucket"
     :initarg :name
     :documentation "the name of the bucket")
   (size
     :reader size
     :type integer
     :initform 0
     :initarg :size
     :documentation "the size of the bucket")
   (amount 
     :accessor amount
     :type integer
     :initform 0
     :documentation "the ammount currently in the bucket"))
  (:documentation "A class to represent a bucket"))

(defmethod print-object ((b bucket) stream)
  "the default print-object method for a bucket"
  (print-unreadable-object (b stream :type t :identity t)
    (with-slots (name size amount) b
      (format stream "name: ~a, size: ~a, amount: ~a" name size amount))))

(defmethod capacity ((b bucket)) (- (size b) (amount b)))

(defmethod fullp ((b bucket)) (= (amount b) (size b)))

(defmethod emptyp ((b bucket)) (zerop (amount b)))

(defmethod fill ((b bucket)) (setf (amount b) (size b)))

(defmethod empty ((b bucket)) (setf (amount b) 0))

(defmethod pour (&key from into)
  (let ((quantity (min (amount from) (capacity into))))
    (decf (amount from) quantity)
    (incf (amount into) quantity)))
