(defpackage :log-levels
  (:use :cl)
  (:export :log-message :log-severity :log-format))

(in-package :log-levels)

;; 0123456789
;; [info]: hello

(defun log-message (log-string) (subseq log-string 8))

(defun log-severity (log-string)
  (let ((sev (string-downcase (subseq log-string 1 5))))
    ;(print (list log-string sev))
    (cond
      ((string= sev "info") :everything-ok)
      ((string= sev "warn") :getting-worried)
      ((string= sev "ohno") :run-for-cover))))

(defun log-format (log-string)
  (case (log-severity log-string)
    (:everything-ok   (string-downcase   (log-message log-string)))
    (:getting-worried (string-capitalize (log-message log-string)))
    (:run-for-cover   (string-upcase     (log-message log-string)))))
