(load "./lib/string-utils")
(load "./lib/math-utils")

(defpackage :affine-cipher
  (:use :cl
        :string-utils :math-utils)
  (:export :encode
           :decode))

(in-package :affine-cipher)

(defvar +alphabet+ "abcdefghijklmnopqrstuvwxyz")
(defvar +m+ (length +alphabet+))

(defun encrypt-letter (letter func)
  (let ((i (position letter +alphabet+)))
    (elt +alphabet+ (funcall func i))))

(defun encrypt (text func)
  (loop for c across (string-downcase text)
        if (alpha-char-p c)
          collect (encrypt-letter c func) into encrypted
        if (digit-char-p c)
          collect c into encrypted
        finally (return (concatenate 'string encrypted))))

(defun encode (plaintext a b)
  (when (= 1 (gcd a +m+))
    (flet ((E (x) (mod (+ (* a x) b) +m+)))
      (string-utils:spaces (encrypt plaintext #'E) :size 5))))

(defun decode (ciphertext a b)
  (when (= 1 (gcd a +m+))
    (flet ((D (y) (mod (* (math-utils:mmi a +m+) (- y b)) +m+)))
      (encrypt ciphertext #'D))))
