(defpackage :food-chain
  (:use :cl)
  (:export :recite))

(in-package :food-chain)

(defvar *animals* #(:fly :spider :bird :cat :dog :goat :cow :horse))

(defun i-know (animal) (format nil "I know an old lady who swallowed a ~(~a~)." animal))

(defvar i-dont-know "I don't know why she swallowed the fly. Perhaps she'll die.")

(defun action (animal)
  (case animal
    (:spider "It wriggled and jiggled and tickled inside her.")
    (:bird "How absurd to swallow a bird!")
    (:cat "Imagine that, to swallow a cat!")
    (:dog "What a hog, to swallow a dog!")
    (:goat "Just opened her throat and swallowed a goat!")
    (:cow "I don't know how she swallowed a cow!")
    (:horse "She's dead, of course!")))

(defun that-wriggled (animal)
  (case animal
    (:spider " that wriggled and jiggled and tickled inside her")
    (otherwise "")))

(defun she-swallowed (predator prey)
  (format nil "She swallowed the ~(~a~) to catch the ~(~a~)~a."
              predator
              prey
              (that-wriggled prey)))

(defun verse (i)
  (let* ((animal (elt *animals* i))
         (action (action animal))
         (lines '()))
    (push (i-know animal) lines)
    (when action (push action lines))
    (unless (eq animal :horse)
            (do ((j i (1- j)))
                ((= j 0))
              (push (she-swallowed (elt *animals* j) (elt *animals* (1- j))) lines))
            (push i-dont-know lines))
    (format nil "~{~a~^~%~}" (reverse lines))))

(defun recite (start-verse end-verse)
  (loop :for i :from start-verse :upto end-verse
        :collect (verse (1- i)) :into verses
        :finally (return (format nil "~{~a~^~%~%~}" verses))))
