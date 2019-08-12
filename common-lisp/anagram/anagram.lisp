(in-package #:cl-user)
(defpackage #:anagram
  (:use #:common-lisp)
  (:export #:anagrams-for))

(in-package #:anagram)

(defun sorted (word)
  (sort (string-downcase (copy-seq word)) #'char<))

(defun anagrams-for (word candidates)
  (let ((key (sorted word)))
    (mapcar #'(lambda (pair) (cdr pair))
      (remove-if-not 
        #'(lambda (pair) (and (string= key (car pair)) 
                              (not (string-equal word (cdr pair)))))
        (mapcar #'(lambda (w) (cons (sorted w) w)) candidates)))))

;; first take, a loop with several variables
; (defun anagrams-for (word candidates)
;  (loop
;     with key     = (sorted word)
;     for candidate in candidates
;     for cand-key = (sorted candidate)
;     when (and (string= key cand-key) (not (string-equal word candidate)))
;       collect candidate))
