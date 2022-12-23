(defpackage :saddle-points
  (:use :cl)
  (:export :saddle-points))

(in-package :saddle-points)

(defun saddle-points (matrix)
  (destructuring-bind (nrows ncols) (array-dimensions matrix)
    (let ((row-maxima (make-array nrows :initial-element most-negative-fixnum))
          (col-minima (make-array ncols :initial-element most-positive-fixnum))
          (saddle-points '()))
      ;; iterate over the matrix to find the minima, maxima
      (loop :for r :below nrows
            :do (loop :for c :below ncols
                      :for val = (aref matrix r c)
                      :do (setf (elt row-maxima r) (max val (elt row-maxima r)))
                      :do (setf (elt col-minima c) (min val (elt col-minima c)))))
      ;; iterate over the matrix to find the saddle points
      (loop :for r :below nrows
            :do (loop :for c :below ncols
                      :for val = (aref matrix r c)
                      :when (and (= val (elt row-maxima r))
                                 (= val (elt col-minima c)))
                        :do (push (list (1+ r) (1+ c)) saddle-points)))
      saddle-points)))
