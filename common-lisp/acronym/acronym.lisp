(in-package #:cl-user)
(defpackage #:acronym
  (:use #:common-lisp)
  (:export #:acronym))

(in-package #:acronym)

;; playing with macros
(defmacro string-buffer () 
  `(make-adjustable-string ""))

(defmacro buf-append (buf c) 
  `(vector-push-extend ,c ,buf))

;;  https://stackoverflow.com/q/18045842/7552
(defun make-adjustable-string (s)
  (make-array (length s)
    :fill-pointer (length s)
    :adjustable t
    :initial-contents s
    :element-type (array-element-type s)))

;; A little state machine: iterating over the string,
;; - if we're looking for an alpha and we find one,
;;   append that char to the acronym and
;;   set the state to "non alpha"
;; - if we're looking for a non-alpha and we find one,
;;   set the state to "alpha"

(defun acronym (phrase)
  (let*
      ((acronym (string-buffer)) 
       (state :ALPHA))
    (dotimes (i (length phrase))
      (let ((c (char phrase i)))
        (cond
          ((and (eql state :ALPHA)
                (alpha-char-p c))
            (buf-append acronym c)
            (setf state :NON-ALPHA))
          ((and (eql state :NON-ALPHA)
                (not (alpha-char-p c))) 
            (setf state :ALPHA)))))
    (string-upcase acronym)))
