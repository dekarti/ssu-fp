#lang racket

(let ([L '(A (B (C D (X E)) F G) H)])
  (car
    (cddr
      (car
        (cdr 
          (car 
            (cdr L)))))))

; (A (B (C D (X E)) F G) H) cdr
; ((B (C D (X E)) F G) H) car
; (B (C D (X E)) F G) cdr
; ((C D (X E)) F G) car
; (C D (X E)) cddr
; ((X E)) caar
; X
