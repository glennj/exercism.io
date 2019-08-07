(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)


(defun build-robot ()
  (make-instance 'robot))


(defclass robot ()
  ((name
    :initform nil
    :reader robot-name
    :documentation "Name of robot instance")
   (all-names
    :initform ()
    :allocation :class
    :documentation "All robot names currently in use.")))

(defmethod initialize-instance :after ((r robot) &key)
  (reset-name r))


(defgeneric reset-name (r)
  (:documentation "Give the robot a new name."))

(defmethod reset-name ((r robot))
  ;; remove the existing name from the all-names set
  (with-slots (name all-names) r
    (when name
      (setf all-names (set-exclusive-or (list name) all-names))))
  ;; and create a new name
  (loop
    for new-name = (random-name)
    while (member new-name (slot-value r 'all-names))
    finally
      (with-slots (all-names) r
        (setf (slot-value r 'name) new-name)
        (setf all-names (adjoin new-name all-names))
        (return r))))


(defun random-name ()
  (coerce
    (vector
      (random-letter)
      (random-letter)
      (random-number)
      (random-number)
      (random-number))
    'string))

(defun random-letter ()
  "a random character between A..Z"
  (code-char (+ 65 (random 26))))

(defun random-number ()
  "a random character between 0..9"
  (code-char (+ 48 (random 10))))
