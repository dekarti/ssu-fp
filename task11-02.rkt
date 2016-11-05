#lang racket

;- inserts element 'e' into list 'L' at position 'i'
(define (paste L e i) 
  (define (paste-iter L cur N)
    (if (null? L) N
      (if (= cur i)
        (paste-iter L (+ cur 1) (append N (cons e '())))
        (paste-iter (cdr L) (+ cur 1) (append N (cons (car L) '()))))))
  (paste-iter L 0 '()))

;- moves i1 element into i2
(define (move L i1 i2)
  (define (get-iter L cur e N)
    (if (= cur i1) 
      (list (cdr L) (car L) N)
      (get-iter (cdr L) (+ cur 1) e (append N (cons (car L) '())))))
  
  (define (paste-iter L2 e L1)
    (let ((delta (- i2 i1)))
      (if (> delta 0) (append L1 (paste L2 e delta))
        (append (paste L1 e i2) L2)))) 

  (apply paste-iter (get-iter L 0 1 '())))
    

(define (reorder1 L i1 k i2)
  (if (= k 0) L
    (reorder1 (move L (+ i1 (- k 1)) (+ i2 (- k 1)))
             i1 (- k 1) i2)))

(define (reorder2 L i1 cur k i2)
  (if (= k cur) L
    (reorder2 (move L (+ i1 cur) (+ i2 cur))
             i1 (+ cur 1) k i2)))

(define (reorder L i1 k i2)
  (let ((delta (- i2 i1)))
    (if (> delta 0)
      (reorder1 L i1 k i2)
      (reorder2 L i1 0 k i2))))


(reorder '(0 1 2 3 4 5 6) 2 2 3) ;- (0 1 4 2 3 5 6)
(reorder '(0 1 2 3 4 5 6) 0 3 3) ;- (3 4 5 0 1 2 6) 
(reorder '(0 1 2 3 4 5 6) 3 2 0) ;- (0 1 4 2 3 5 6)
