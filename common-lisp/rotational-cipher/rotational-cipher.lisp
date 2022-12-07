(defpackage :rotational-cipher
  (:use :cl)
  (:export :rotate))

(in-package :rotational-cipher)

(defun rotate (text key)
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (k (mod key (length alphabet)))
         (rotated (concatenate 'string 
                               (subseq alphabet k)
                               (subseq alphabet 0 k))))
    (flet ((encode (c)
            (if (alpha-char-p c)
                (let* ((lc (char-downcase c))
                       (idx (position lc alphabet))
                       (encrypted (elt rotated idx))
                       (upcase (if (upper-case-p c) #'char-upcase #'identity)))
                  (funcall upcase encrypted))
                c)))

      (map 'string #'encode text))))
