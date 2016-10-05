#lang racket
(define (func L1 L2 L3)
  (if (null? L1) #f 
      (let ((lastL1 (last L1)))
        (cond 
          ((number? lastL1)         (list L2 L3))
          ((equal? lastL1 (car L1)) (cons L2 (cdr L1)))
          (else                     (cons L3 (cdr L1)))))))

(func '() 5 6)
(func '(1 2 3 4) 5 6)
(func '((4) 2 3 (4)) 5 6)
(func '(1 2 3 (4)) 5 6)
