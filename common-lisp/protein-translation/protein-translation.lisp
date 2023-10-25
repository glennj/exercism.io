(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(define-condition invalid-protein (error) ())

(defun codon-protein (codon)
  (cond
    ((string= codon "AUG") "Methionine")
    ((string= codon "UGG") "Tryptophan")
    ((member codon '("UUU" "UUC") :test #'string=) "Phenylalanine")
    ((member codon '("UUA" "UUG") :test #'string=) "Leucine")
    ((member codon '("UCU" "UCC" "UCA" "UCG") :test #'string=) "Serine")
    ((member codon '("UAU" "UAC") :test #'string=) "Tyrosine")
    ((member codon '("UGU" "UGC") :test #'string=) "Cysteine")
    ((member codon '("UAA" "UAG" "UGA") :test #'string=) "STOP")
    (t (error 'invalid-protein))))

(defun proteins (strand)
  (labels ((helper (strand result)
            (case (length strand)
              (0 result)
              ((1 2) (error 'invalid-protein))
              (otherwise
                (let ((protein (codon-protein (subseq strand 0 3))))
                  (if (string= protein "STOP")
                      result
                      (helper (subseq strand 3) (push protein result))))))))
    (reverse (helper strand '()))))
