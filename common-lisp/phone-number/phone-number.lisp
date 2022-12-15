(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defun remove-valid-non-digits (phone)
  (remove-if #'(lambda (c) (position c " ()-+.")) phone))

(defun only-digits (phone) (every #'digit-char-p phone))

(defun valid-length (phone)
  (case (length phone)
    (10 t)
    (11 (char= #\1 (elt phone 0)))))

(defun strip-country-code (phone)
  (subseq phone (- (length phone) 10)))

(defun valid-digit (phone &key idx)
  (let ((digit (elt phone idx)))
    (not (or (char= digit #\0)
             (char= digit #\1)))))

(defun valid-area-code (phone) (valid-digit phone :idx 0))
(defun valid-exchange  (phone) (valid-digit phone :idx 3))

(defun cleaned (phrase)
  (let ((phone (remove-valid-non-digits phrase)))
    (when (and (only-digits phone)
               (valid-length phone))
      (let ((phone (strip-country-code phone)))
        (when (and (valid-area-code phone)
                   (valid-exchange phone))
          phone)))))

(defun clean (phrase)
  (let ((phone (cleaned phrase)))
    (if phone phone "0000000000")))
