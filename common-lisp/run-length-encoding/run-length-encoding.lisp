;;;; Run-length Encoding
;;;;
;;;; implementing `encode` and `decode` with recursive local functions
;;;; to consume the input character-by-character
;;;;
;;;; this is way easier with regular expressions...
;;;; https://exercism.org/tracks/kotlin/exercises/run-length-encoding/solutions/glennj

(load "lib/string-utils")

(defpackage :run-length-encoding
  (:use :cl
        :string-utils)
  (:export :encode
           :decode))

(in-package :run-length-encoding)

(defun concat (str num char)
  "convenience function to smush strings together"
  (if (zerop num)
      str
      (format nil "~a~a~a" str (if (= 1 num) "" num) char)))

(defun encode (plain)
  "run-length encode a plaintext string"
  (labels
    ((helper (text prev run compressed)
      (multiple-value-bind (c text) (string-utils:string-pop text)
        (if (null c)
            (concat compressed run prev)
            (if (char= c prev)
                (helper text c (1+ run) compressed)
                (helper text c 1 (concat compressed run prev)))))))
    (helper plain #\Nul 0 "")))

(defun decode (compressed)
  "run-length decode an encoded string"
  (labels
    ((helper (text run expanded)
      (multiple-value-bind (c text) (string-utils:string-pop text)
        (if (null c)
            expanded
            (if (digit-char-p c)
                (let ((run (+ (* run 10) (string-utils:char-digit-value c))))
                  (helper text run expanded))
                (let ((cs (string-utils:string-repeat c (if (zerop run) 1 run))))
                  (helper text 0 (concat expanded 1 cs))))))))
    (helper compressed 0 "")))
