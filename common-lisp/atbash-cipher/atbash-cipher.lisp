(load "lib/string-utils")

(defpackage :atbash-cipher
  (:use :cl
        :string-utils)
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

(defun encode (plaintext)
  (loop for letter across (string-downcase plaintext)
        if (alphanumericp letter)
          collect (encipher letter) into encoded
        finally
          (return (string-utils:spaces (concatenate 'string encoded)
                                       :size 5))))
