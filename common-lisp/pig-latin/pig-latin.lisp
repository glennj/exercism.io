(load "lib/string-utils")

(defpackage :pig-latin
  (:use :cl
        :string-utils)
  (:export :translate))

(in-package :pig-latin)

(defun pigify (word pos)
  (format nil "~a~aay" (subseq word pos)
                       (subseq word 0 pos)))

(defun translate-word (word)
  (let ((vowel-pos (position-if #'(lambda (c) (position c "aeiou")) word))
        (y-pos (position #\y word))
        (q-pos (position #\q word)))
    (cond

      ;; first vowel is a y
      ((and y-pos
            (> y-pos 0)
            (or (null vowel-pos)
                (< y-pos vowel-pos)))
         (pigify word y-pos))
        
      ;; starts with a vowel sound (or no vowels)
      ((or (null vowel-pos)
           (= vowel-pos 0)
           (string= "xr" (subseq word 0 2))
           (string= "yt" (subseq word 0 2)))
        (pigify word 0))

      ;; starts with qu
      ((and (char= #\u (elt word vowel-pos))
            q-pos
            (= q-pos (1- vowel-pos)))
        (pigify word (1+ vowel-pos)))

      ;; starts with consonant(s)
      (t (pigify word vowel-pos)))))

(defun translate (phrase)
  (loop for word in (string-utils:split-string phrase)
        collect (translate-word word) into ordsway
        finally (return (string-utils:join-strings ordsway))))
