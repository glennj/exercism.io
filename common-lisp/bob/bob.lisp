(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response))
(in-package #:bob)


(defun whitespace-char-p (c)
  ;; too bad Space is a graphic char.
  (or (char= #\Space c) (not (graphic-char-p c))))

(defun silentp (input)
  (every #'whitespace-char-p input))

(defun questionp (input)
  (let ((inp (string-right-trim " " input)))
    (and
      (> (length inp) 0)
      (char= #\? (char inp (1- (length inp)))))))

(defun yellingp (input)
  (and 
    (some   #'upper-case-p input)
    (notany #'lower-case-p input)))


(defun response (input)
  (let ((S (silentp   input))
        (Q (questionp input)) 
        (Y (yellingp  input)))
    (cond
      (S         "Fine. Be that way!")
      ((and Q Y) "Calm down, I know what I'm doing!")
      (Q         "Sure.")
      (Y         "Whoa, chill out!")
      (t         "Whatever."))))
