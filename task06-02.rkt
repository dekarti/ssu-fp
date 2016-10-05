#lang racket 

-; (G55 G66 G777), (9 (F G) I), (N I L T D J (II JJ)),
-; 3, 2, 6.

(let ((L1 '(G55 G66 G777))
      (L2 '(9 (F G) I))
      (L3 '(N I L T D J (II JJ))))
  (and
    (number? (caddr L1))
    (not (pair? (cadr L2))
    (list? (cadr (cddddr L3))))))
