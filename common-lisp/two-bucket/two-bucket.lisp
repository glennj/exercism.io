(load "bucket")

(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)

(defun valid (goal size1 size2) 
  (and (<= goal (max size1 size2))
       (let ((gcd_ (gcd size1 size2)))
         (or (= 1 gcd_)
             (zerop (mod goal gcd_))))))

(defun goal-satisfied (goal-bucket other-bucket moves)
  (list (cons :moves moves)
        (cons :goal-bucket (bucket:name goal-bucket))
        (cons :other-bucket (bucket:amount other-bucket))))

(defun solve (goal b1 b2 &optional (moves 0))
  (when (zerop moves)
    (bucket:fill b1)
    (incf moves))
  (when (and (= 1 moves) (= goal (bucket:size b2)))
    (bucket:fill b2)
    (incf moves))
  (cond
    ((= goal (bucket:amount b1)) (goal-satisfied b1 b2 moves))
    ((= goal (bucket:amount b2)) (goal-satisfied b2 b1 moves))
    (t (cond
         ((bucket:emptyp b1) (bucket:fill b1))
         ((bucket:fullp  b2) (bucket:empty b2))
         (t                  (bucket:pour :from b1 :into b2)))
       (solve goal b1 b2 (1+ moves)))))

(defun measure (bucket-one bucket-two goal start-bucket)
  "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an alist of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
  (when (valid goal bucket-one bucket-two)
    (let ((b1 (make-instance 'bucket:bucket :name :one :size bucket-one))
          (b2 (make-instance 'bucket:bucket :name :two :size bucket-two)))
      (if (equal start-bucket :one)
          (solve goal b1 b2)
          (solve goal b2 b1)))))
