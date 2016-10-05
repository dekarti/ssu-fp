#lang racket

(define (insert L position element)
  (if (= 0 position)
      (cons element L)
      (cons (car L) 
            (insert (cdr L) (- position 1) element))))
 
(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))
 
(define (perms l)
  (if (null? l) '(())
      (apply append (map (lambda (p)
                           (map (lambda (n)
                                  (insert p n (car l)))
                                (seq 0 (length p))))
                         (perms (cdr l))))))

(perms '(1 2 3 4))
