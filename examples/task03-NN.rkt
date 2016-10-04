#lang racket
# (A (B) () () (C D (E)) (F () (X ())) G H)

(cons 'A
      (cons 
       (cons 'B '())
       (cons '()
             (cons '()
                   (cons 
                    (cons 'C
                          (cons 'D
                                (cons (cons 'E '()) '())))
                    (cons 
                     (cons 'F
                           (cons '()
                                 (cons 
                                  (cons 'X 
                                        (cons '() '())) '())))
                     (cons 'G (cons 'H '()))))))))

