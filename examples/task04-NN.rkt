#lang racket

(let ((L1 '(1 2 3 4 5 6 7 8 9))
      (L2 '(1 2 3 4 5 6 7 8 9))
      (L3 '(1 2 3 4 5 6 7 8 9 10)))
  (list
   (cadddr (cddddr L1))          ; восьмой элемент из L1
   (car (cddddr (cddddr L2)))    ; девятый элемент из L2
   (cadr (cddddr (cddddr L3))))) ; десятый элемент из L3
  