(defmodule grains
  (export (square 1))
  (export (total 0)))

(defun square (n)
  (2-to-the-power (- n 1)))

(defun total ()
  (- (2-to-the-power 64) 1))

(defun 2-to-the-power (e)
  (bsl 1 e))
