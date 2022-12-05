(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode))

(in-package :atbash-cipher)

(defun encipher (alphanumeric)
  "Encipher a single character atbash-style"
  (cond
    ((digit-char-p alphanumeric) alphanumeric)
    ((alpha-char-p alphanumeric)
       (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
              (idx (position alphanumeric alphabet))
              (rev-idx (- (length alphabet) (+ idx 1))))
         (elt alphabet rev-idx)))))

(defun add-word (str word) 
  "Concatenate a space and a word to a string"
  (string-left-trim " " (concatenate 'string str " " word)))

(defun spaces (str &key (result "") (size 5))
  "Add a space after each 5th character in a string"
  (if (<= (length str) size)
      (add-word result str)
      (spaces (subseq str size)
              :result (add-word result (subseq str 0 size))
              :size size)))


(defun encode (plaintext)
  (loop for letter across (string-downcase plaintext)
        if (alphanumericp letter)
          collect (encipher letter) into encoded
        finally
          (return (spaces (concatenate 'string encoded)))))
