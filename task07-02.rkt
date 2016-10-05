#lang racket

(define (func A)
  (cond
    ((number? A) (* A A))
    ((list? A) (cdr A))
    (else A)))

(func 5)
(func '(1 2 3))
(func "Arsen")

