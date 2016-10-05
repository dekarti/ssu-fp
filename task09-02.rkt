#lang racket

(define (func L1)
  (define (sum L2 s)
    (cond ((empty? L2) s)
          (else (sum (cdr L2) (+ s (car L2))))))
  (define (gen L3 L)
    (cond ((empty? L3) L)
          (else 
            (gen (cdr L3)
                 (append L (list (sum L3 0)))))))
  (gen L1 '()))

(func '(4 5 6 7))

