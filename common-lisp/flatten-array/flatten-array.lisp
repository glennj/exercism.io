(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun flatten (nested)
  (labels ((flattener (source flattened)
             (dolist (elem source)
                     (if (listp elem)
                         (setf flattened (flattener elem flattened))
                         (when elem (push elem flattened))))
             flattened))
    (nreverse (flattener nested '()))))
