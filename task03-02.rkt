#lang racket
-; (A (B (C D (X E)) F G) H)

(let ((2nd
      (cons 'B
            (cons (cons 'C
                        (cons 'D
                              (cons (cons 'X (cons 'E '())) '())))
                  (cons 'F (cons 'G '()))))))

(cons 'A (cons 2nd (cons 'H '()))))
