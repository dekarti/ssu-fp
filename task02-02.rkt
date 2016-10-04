#lang racket

(let ([L '(A (B (C D (X E)) F G) H)])
  (car
    (cddr
      (car
        (cdr 
          (car 
            (cdr L)))))))

