(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

;;; Using the algorithm spelled out at
;;; https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem

(defun maximum-value (maximum-weight items)
  (let* ((n (length items))
         (w (mapcar #'(lambda (item) (cdr (assoc :weight item))) items))
         (v (mapcar #'(lambda (item) (cdr (assoc :value item))) items))
         (value (make-array (list (1+ n) (1+ maximum-weight)) :initial-element 0)))

    ;; populate the value matrix
    (loop :for i :from 1 :upto n
          :do (loop :for j :from 0 :upto maximum-weight
                    :do (setf (aref value i j)
                              (if (> (nth (1- i) w) j)
                                  (aref value (1- i) j)
                                  (max (aref value (1- i) j)
                                       (+ (nth (1- i) v)
                                          (aref value (1- i)
                                                      (- j (nth (1- i) w)))))))))

    ;; return the maximum value
    (aref value n maximum-weight)))
