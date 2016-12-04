#lang racket

;; Split file content into lists of lists
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

;; (get-value "K" '("A" "5" "B" "4" "C" "6")) -> "NULL"
;; (get-value "B" '("A" "5" "B" "4" "C" "6")) -> "4"
(define (get-value E L)
  (if (null? L) "NULL"
    (if (string=? E (car L)) (cadr L)
      (get-value E (cddr L)))))

;; Input: '("A" "5" "B" "6") '("A" "B" "C") 
;; Output ("A" "5" "B" "6" "C" "NULL")
(define (maximize KV K)
  (define (iter L keys)
    (if (null? keys) L
      (iter (append L
                    (list (car keys)
                          (get-value (car keys) KV)))
            (cdr keys))))
  (iter '() K))

(define (normalize parameters keys)
  (define (iter P result)
    (if (null? P) result
     (iter (cdr P)
      (append result
              (list (cons (caar P)
                  (maximize (cdar P) keys)))))))
  (iter parameters '()))

(define (create-protocol file)
  (let* ([parameters (file->list file)]
         [key-values (map cdr parameters)]
         [info (get-info parameters)]
         [keys (cadr info)])
    (call-with-output-file "protocol"
      (lambda (out)
        (let f ((L (normalize parameters keys)))
          (if (null? L) (newline out)
            (begin
              (let g ((P (car L)))
                (if (null? P) (newline out)
                  (begin
                    (display (car P) out)
                    (display " " out)
                    (g (cdr P)))))
              (f (cdr L)))))))))

;(get-value "K" '("A" "5" "B" "4" "C" "6"))

;(maximize '("A" "5" "B" "6") '("A" "B" "C"))
(normalize '(("1000" "a" "12" "b" "13") ("1100" "a" "16" "c" "17") ("1200" "f" "12" "c" "19" "m" "12") ("1300" "a" "22" "b" "11" "f" "3")) '("a" "b" "c" "f" "m"))
(create-protocol "parameters")
;-(get-info (file->list "parameters"))
