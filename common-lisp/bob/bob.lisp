(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response-for))
(in-package #:bob)

(defun response-for (input)
  (if (silentp input) 
    "Fine. Be that way!"
    (let ((Q (questionp input)) 
          (Y (yellingp  input)))
      (cond
        ((and Q Y) "Calm down, I know what I'm doing!")
        (Q "Sure.")
        (Y "Whoa, chill out!")
        (t "Whatever.")))))

(defun silentp (input)
  (every #'whitespace-char-p input))

(defun whitespace-char-p (c)
  ;; too bad Space is a graphic char.
  (or (char= #\Space c) (not (graphic-char-p c))))

(defun questionp (input)
  (let ((inp (string-right-trim " " input)))
    (char= #\? (char inp (1- (length inp))))))

(defun yellingp (input)
  (and 
    (some   #'upper-case-p input)
    (notany #'lower-case-p input)))
