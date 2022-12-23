;; See my julia solution for the rationale.
;; https://exercism.org/tracks/julia/exercises/spiral-matrix/solutions/glennj

(defpackage :spiral-matrix
  (:use :cl)
  (:export :spiral-matrix))

(in-package :spiral-matrix)

;; This function takes some values, creates a circular list with them,
;; and returns a function that outputs the _next_ element in the list.
(defun cycle (&rest elems)
  (setf (cdr (last elems)) elems)
  (lambda () (pop elems)))

(defun spiral-matrix (size)
  (when (plusp size)
    (loop :with spiral = (make-array (list size size))
          :with next-Δidx = (cycle 1 size -1 (- size))
          :with next-Δtimes = (cycle 1 0)
          :with idx = -1
          :with times = size
          :with i = 1
          :while (<= i (* size size))
          :for Δidx = (funcall next-Δidx)
          :for Δtimes = (funcall next-Δtimes)
          :do (loop :repeat times
                    :do (incf idx Δidx)
                    :do (setf (row-major-aref spiral idx) i)
                    :do (incf i))
          :do (decf times Δtimes)
          :finally (return spiral))))
