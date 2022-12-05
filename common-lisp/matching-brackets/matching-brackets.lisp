(defpackage :matching-brackets
  (:use :cl)
  (:export :pairedp))

(in-package :matching-brackets)

(defun matches (opener closer)
  (or
    (and (char= #\( opener) (char= #\) closer))
    (and (char= #\[ opener) (char= #\] closer))
    (and (char= #\{ opener) (char= #\} closer))))

(defun pairedp (value &optional (stack '()))
  (if (string= "" value)
      (null stack)
      (let ((char (elt value 0))
            (rest (subseq value 1)))
        (case char
          ((#\( #\[ #\{) (pairedp rest (push char stack)))
          ((#\) #\] #\}) (when (and (not (null stack))
                                    (matches (pop stack) char))
                               (pairedp rest stack)))
          (otherwise (pairedp rest stack))))))
