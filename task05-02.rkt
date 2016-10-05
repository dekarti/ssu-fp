#lang racket

-; (G55 G66 G777), (9 (F G) I), (N I L T D J (II JJ))
-; 3, 2, 4

(let ((L1 '(G55 G66 G777))
      (L2 '(9 (F G) I))
      (L3 '(N I L T D J (II JJ))))
  (append
    (list (car L1) (cadr L1))
    (list (car L2))
    (cddr L2)
    (list
      (car L3)
      (cadr L3)
      (caddr L3))
    (cddddr L3)))

