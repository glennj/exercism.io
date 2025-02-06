(define (hamming-distance strand-a strand-b)
  (letrec ((hammer (lambda (dist chars-a chars-b)
                     (if (null? chars-a)
                         dist
                         (let ((delta (if (char=? (car chars-a) (car chars-b)) 0 1)))
                           (hammer (+ dist delta) (cdr chars-a) (cdr chars-b)))))))

    (if (not (= (string-length strand-a) (string-length strand-b)))
        (raise-exception "strand lengths not equal")
        (hammer 0 (string->list strand-a) (string->list strand-b)))))

