(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

;; ---------------------------------------------------------
;; an iterative solution
;; - this traverses the input string several times

(defun isbn-sum (isbn)
  (loop for c across isbn
        for i = 10 then (1- i)
        if (and (= i 1) (char= #\X c))
          sum 10
        else
          sum (* i (digit-char-p c))))

(defun valid-chars-p (isbn)
  (loop for c across isbn
        for i = 10 then (1- i)
        when (and (> i 1) (not (digit-char-p c)))
          do (return nil)
        finally (return (or (digit-char-p c) (char= #\X c)))))

(defun validp-iterative (isbn)
  (let ((cleaned (remove #\- isbn)))
    (when (and (= 10 (length cleaned))
               (valid-chars-p cleaned))
      (zerop (mod (isbn-sum cleaned) 11)))))

;; ---------------------------------------------------------
;; a more concise iterative solution

(defun isbn-digit-value (c i)
  "Return the digit value of this character at this index.
   Invalid characters return nil."
  (if (and (= 1 i) (char= #\X c))
      10
      (digit-char-p c)))

(defun validp-iterative2 (isbn)
  (loop with d
        for c across (remove #\- isbn)
        for i = 10 then (1- i)
        do (setf d (isbn-digit-value c i))
        unless d do (return nil)
        count d into n
        sum (* i d) into total
        finally
          (return (and (= 10 n)
                       (zerop (mod total 11))))))

;; ---------------------------------------------------------
;; a recursive solution
;; - it examines each char in the input string just once

(defun validp-recursive (isbn &optional (i 10) (sum 0))
  (cond
    ((minusp i)
      ;; isbn string too long
      nil)
    ((string= "" isbn)
      ;; when then string is empty, we must have
      ;; - counted exactly 10 digits (i is zero)
      ;; - the sum mod 11 is zero
      (and (zerop i) (zerop (mod sum 11))))
    (t
      (let ((c (elt isbn 0)))
        (if (char= #\- c)
            ;; ignore hyphens
            (validp-recursive (subseq isbn 1) i sum)
            ;; otherwise
            (let ((d (isbn-digit-value c i)))
              (when d (validp-recursive (subseq isbn 1) (1- i) (+ sum (* i d))))))))))

;; ---------------------------------------------------------
(defun validp (isbn)
  "Determine if the given ISBN string is valid."
  ;(validp-iterative isbn))
  (validp-iterative2 isbn))
  ;(validp-recursive isbn))
