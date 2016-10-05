#lang racket

; (sortList L lessThan) - сортировка списка методом пузырька
; L - заданный список, lessThan - функция сравнения, 
; в соответствии с которой проводить сортировку
(define (sortList L lessThan)
  (define (bublesort unsorted X L sorted)
    (if (null? L) 
        (let ((newsorted (cons X sorted))
              (newL (reverse unsorted)))
          (if (null? newL) newsorted
              (bublesort '() (car newL) (cdr newL) newsorted)))
        (let ((Y (car L)))
          (if (lessThan X Y)
              (bublesort (cons X unsorted) Y (cdr L) sorted)
              (bublesort (cons Y unsorted) X (cdr L) sorted)))))
  (if (null? L) L
      (bublesort '() (car L) (cdr L) '())))

; основная функция 
(define (func L)
  ;(sortString S) - сортировка символов в строке S:
  ; провращение строки в список символов, 
  ; сортировка списка символов с помощью функции char<?
  ; и превращение списка в строку
  (define (sortString S)
    (list->string (sortList (string->list S) char<?)))
  (define (func-iter L rez)
    (if (null? L) 
        (reverse rez)
        (let ((fstL (car L)))
          (if (string? fstL)
              (func-iter (cdr L)
                         (cons (sortString fstL) rez))
              (func-iter (cdr L) (cons fstL rez))))))
  (func-iter L '()))

(func '("" "nfhwfla" 25 "AkgdsAufGFD" (1 2 3 4 6) 56 "jeurlA" abc))