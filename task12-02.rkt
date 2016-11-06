#lang racket

(define (file->list file)
  (map (lambda (x) (string-split x " ")) 
       (string-split (file->string file) "\n")))

(define (remove-each-second L result)
  (if (null? L) result
    (remove-each-second (cddr L) (cons (car L) result))))

(define (get-info L) 
  (define (iter K times keys)
    (if (null? K) (list times keys)
      (iter (cdr K) 
            (append times (cons (caar K) '()))
            (append keys (cdr (car K))))))
  (let ([P (iter L '() '())])
    (list (car P)
          (remove-duplicates (remove-each-second (cadr P) '())))))

(define (create-protocol file)
  (let* ([parameters (file->list file)]
         [key-values (map cdr parameters)]
         [info (get-info parameters)]
         [times (car info)]
         [keys (cadr info)])
    key-values))

(create-protocol "parameters")
;-(get-info (file->list "parameters"))
