(in-package #:cl-user)
(defpackage #:word-count
  (:use #:cl)
  (:export #:count-words))

(in-package #:word-count)

(defun count-words (sentence)
  (loop
    with word = ""
    with word-counts = '()
    for c across sentence
    if (word-char-p c)
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


(defun word-char-p (c)
  (or (alphanumericp c)
      (char= #\' c)))

(defun string-append (str c)
  (concatenate 'string str (list c)))

(defun incr-word (word counts)
  "Given an association list of (word . count), incr the count."
  (let* ((word (string-trim '(#\') word))
         (pair (assoc word counts :test #'string=)))
    (if (null pair)
      (push (cons word 1) counts)
      (incf (cdr pair)))
    counts))
