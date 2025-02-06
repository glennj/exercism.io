(import (rnrs))

(define (square n)
  (if (<= 1 n 64)
      (bitwise-arithmetic-shift-left #b1 (1- n))
      (error 'square "square must be between 1 and 64")))

(define total
  (1- (bitwise-arithmetic-shift-left #b1 64)))
