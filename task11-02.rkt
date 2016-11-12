#lang racket

;; Divides initial list onto two parts
(define (cut L i k) 
  (define (iter L1 cur L2 L3)
    (cond 
      ((< cur i) 
       (iter (append L1 (list (car L3))) (+ cur 1) L2 (cdr L3)))
      ((and (< cur (+ i k)) (>= cur i))
       (iter L1 (+ cur 1) (append L2 (list (car L3))) (cdr L3)))
      (else (list (append L1 L3) L2))))
  (iter '() 0 '() L))

;; Inserts L2 into L1 on position i
(define (insert L1 L2 i)
  (define (divide cur L3 L4)
    (if (= cur i) 
        (list L3 L4)
        (divide (+ cur 1)
                (append L3 (list (car L4)))
                (cdr L4))))
  (let* ([parts (divide 0 '() L1)]
         [first (car parts)]
         [second (cadr parts)])
    (append first L2 second)))

(define (reorder L i1 k i2)
  (cond ((and (>= i2 i1) (< i2 (+ i1 k))) L)
        ((> i1 i2) (apply
          insert (append (cut L i1 k) (list i2))))
        (else (apply
          insert (append (cut L i1 k) (list (- i2 k)))))))


(reorder '(0 1 2 3 4 5 6 7) 2 2 6)
(reorder '(0 1 2 3 4 5 6 7) 2 1 3)
(reorder '(0 1 2 3 4 5 6 7) 5 3 2)
(reorder '(0 1 2 3 4 5 6 7) 2 2 8)
(reorder '(a s d f g h j k) 3 3 2) ; (a s f g h d j k)
