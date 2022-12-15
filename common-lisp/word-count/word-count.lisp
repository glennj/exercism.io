(in-package #:cl-user)
(defpackage #:word-count
  (:use #:cl)
  (:export #:count-words))

(in-package #:word-count)

(defun string-append (str c)
  (concatenate 'string str (list c)))

(defun countable-word-char-p (c)
  (or (alphanumericp c)
      (char= #\' c)))

(defun countable-word-p (word)
  (let ((w (string-trim '(#\') word)))
    (when (string/= "" w) w)))

(defun incr-word (word counts)
  "Given an association list of (word . count), incr the count."
  (let ((w (countable-word-p word)))
    (when w
      (let ((pair (assoc w counts :test #'string=)))
        (if (null pair)
            (push (cons w 1) counts)
            (incf (cdr pair))))))
  counts)

(defun count-words (sentence)
  (loop with word = ""
        with word-counts = '()
        for c across sentence
        if (countable-word-char-p c)
          do (setf word (string-append word (char-downcase c)))
        else
          do (setf word-counts (incr-word word word-counts))
          and do (setf word "")
        finally
          (setf word-counts (incr-word word word-counts))
        finally
          (return word-counts)))
