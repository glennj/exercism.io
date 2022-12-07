(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

(defgeneric add (school name grade))
(defgeneric roster (school))
(defgeneric grade (school grade))

(defclass school ()
  ((student-directory
     :accessor student-directory
     :initform (make-hash-table)
     :type hash-table))
  (:documentation "I keep track of the roster of students at a school"))

(defun make-school () (make-instance 'school))

(defmethod add ((s school) (name string) (grade integer))
  (let ((key (read-from-string name))
        (dir (student-directory s)))
    (unless (second (multiple-value-list (gethash key dir))) 
            (setf (gethash key dir) (list name grade)))))

(defmethod roster ((s school))
  (let* ((students (loop with dir = (student-directory s)
                         for student being the hash-values in dir
                         collect student))
         (by-name (sort students #'(lambda (a b) (string< (first a) (first b)))))
         (by-grade (stable-sort by-name #'(lambda (a b) (< (second a) (second b))))))
    (mapcar #'first by-grade)))

(defmethod grade ((s school) (grade integer))
  (loop for student being the hash-values in (student-directory s)
        if (= grade (second student))
          collect (first student) into classroom
        finally (return (sort classroom #'string<))))
