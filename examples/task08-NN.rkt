#lang racket

(define (y N)
  (define (ij-func i j)
    (+ (/ i j) (/ (* i i) 2)))
  (define (y-iter i j prod sum)
    (cond ((> i N) sum)
          ((> j N) (y-iter (+ i 1) 1 1 (+ sum prod)))
          (else    (y-iter i (+ j 1) (* prod (ij-func i j)) sum))))
  (y-iter 1 1 1 0))
