#lang racket

(let ((L1 '(1 2 3 4 5 6 7 8 9))
      (L2 '(1 2 3 4 5 6 7 8 ()))
      (L3 '(1 2 3 4 5 6 7 8 9 ())))
  (and
   (number? (cadddr (cddddr L1)))         
   (not (pair? (car (cddddr (cddddr L2)))))
   (list? (cadr (cddddr (cddddr L3))))))