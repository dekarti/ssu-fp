#lang racket

(define (y N)
  (define (f num result)
    (cond 
      ((> num 1) (f (- num 1) (* num result)))
      (else result)))
  (define (sum i s)
    (cond ((> i 0) (sum (- i 1) (+ s (f i 1))))   
          (else s)))
  (sum N 0))

(y 5)

