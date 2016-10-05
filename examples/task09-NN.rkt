#lang racket

(define (func L1 L2)
  (define (func-iter L1 L2 rez)
    (if (null? L1) 
        (append (reverse rez) L2)
        (func-iter L2 (cdr L1) (cons (car L1) rez))))
  (func-iter L1 L2 '()))

(func '(1 2 3 4 5 6) '(11 22 33 44 55 66 77 88))
(func '(11 22 33 44 55 66 77 88) '(1 2 3 4 5 6))
(func '(1 2 3 4 5 6) '())
(func '() '(1 2 3 4 5 6))
(func '() '())