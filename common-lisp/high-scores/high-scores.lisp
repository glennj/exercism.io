(defpackage :high-scores
  (:use :cl)
  (:export :make-high-scores-table :add-player
           :set-score :get-score :remove-player))

(in-package :high-scores)

(defun make-high-scores-table () (make-hash-table))

(defun set-score (scores name score) (setf (gethash name scores) score))

(defun add-player (scores name) (set-score scores name 0))

(defun get-score (scores name) (gethash name scores 0))

(defun remove-player (scores name) (remhash name scores))
