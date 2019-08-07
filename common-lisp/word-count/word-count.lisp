(in-package #:cl-user)
(defpackage #:phrase
  (:use #:cl)
  (:export #:word-count))
(in-package #:phrase)

(defun word-count (sentence)
  (loop
    with word = ""
    with word-counts = '()
    for c across sentence
    if (alphanumericp c)
      do (setf word (string-append word (char-downcase c)))
    else
      if (string/= "" word)
        do (setf word-counts (incr-word word word-counts))
        and do (setf word "")
      end
    finally
      (when (string/= "" word)
        (setf word-counts (incr-word word word-counts)))
    finally (return word-counts)))


(defun string-append (str c)
  (concatenate 'string str (list c)))

(defun incr-word (word a-list)
  "Given an association list of (word . count), incr the count."
  (let ((pair (assoc word a-list :test #'string=)))
    (if (null pair)
      (push (cons word 1) a-list)
      (incf (cdr pair)))
    a-list))
