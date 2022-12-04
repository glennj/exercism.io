(defpackage :robot-simulator
  (:use :cl)
  (:export :+north+ :+east+ :+south+ :+west+ :execute-sequence
           :robot :robot-position :robot-bearing :make-robot))

(in-package :robot-simulator)

(defconstant +east+    0)
(defconstant +north+  90)
(defconstant +west+  180)
(defconstant +south+ 270)

(defclass robot () 
  ((position
     :accessor robot-position
     :initarg :position
     :type cons
     :documentation "the robot's current position")
   (bearing
     :accessor robot-bearing
     :initarg :bearing
     :type integer
     :documentation "the robot's current compass bearing"))
  (:documentation "A robot"))

(defun make-robot (&key (position '(0 . 0)) (bearing +north+))
  (make-instance 'robot :position position :bearing bearing))

(defgeneric turn-right (robot))
(defgeneric turn-left (robot))
(defgeneric advance (robot))

(defun turn (robot direction)
  (let ((b (robot-bearing robot)))
    (setf (robot-bearing robot) (mod (+ b (* direction 90)) 360))))

(defmethod turn-right ((robot robot)) (turn robot -1))
(defmethod turn-left  ((robot robot)) (turn robot  1))

(defun delta (robot func)
  (let ((radians (* (robot-bearing robot) (/ pi 180))))
    (round (funcall func radians))))

(defun dx (robot) (delta robot #'cos))
(defun dy (robot) (delta robot #'sin))

(defmethod advance ((robot robot))
  (destructuring-bind (x . y) (robot-position robot)
    (setf (robot-position robot) (cons (+ x (dx robot))
                                       (+ y (dy robot))))))

(defun execute-sequence (robot instructions)
  (unless (string= "" instructions)
    (case (elt instructions 0)
      (#\R (turn-right robot))
      (#\L (turn-left robot))
      (#\A (advance robot)))
    (execute-sequence robot (subseq instructions 1))))
