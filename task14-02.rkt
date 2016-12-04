#lang racket

(define (extra-combinations L1 L2)
  (apply append (map (lambda(x) (combinations L1 x)) L2)))

(define (comp C1 C2)
  (if (equal? (car C1) 'm)
    (if (equal? (car C2) 'k) #t
      (string<? (cdr C1) (cdr C2)))
    (if (equal? (car C2) 'm) #f
      (string<? (cdr C1) (cdr C2)))))

(define (move L1 L2 L D)
  (if (equal? D 'L)
    (list (sort (remove* L L1) comp)
          (sort (append L L2) comp))
    (list (sort (append L L2) comp)
          (sort (remove* L L1) comp))))
  
(define (opposite C)
  (if (equal? C 'R) 'L 'R))

(define (all-ways state)
  (let* ([left (first state)]
         [right (second state)]
         [dir (third state)])
    (if (equal? dir 'L)
        (map (lambda(x)
               (append (move left right x dir)
                       (list (opposite dir))))
             (combinations left 2))
        (map (lambda(x)
               (append (move right left x dir)
                       (list (opposite dir))))
             (extra-combinations right '(1 2))))))
             
(define (is_missioner? person)
  (equal? (car person) 'm))

(define (is_cannibal? person)
  (equal? (car person) 'k))

(define (count predicate L)
  (length (filter predicate L)))
 
(define (save? state)
  (let ([left (first state)]
         [right (second state)])
    (or
      (= (count is_missioner? left) 0)
      (= (count is_missioner? right) 0)
      (and 
        (>= (count is_missioner? left)
            (count is_cannibal? left))
        (>= (count is_missioner? right)
            (count is_cannibal? right))))))

(define (possible-ways ways) (filter save? (all-ways ways)))

(define (extract-path goal state-path path)
  (if (null? goal) path 
      (let ((next-fst (caar state-path))
            (next-snd (cadar state-path)))
        (if (equal? goal next-fst) 
            (if (null? next-snd) path
                (extract-path next-snd 
                              (cdr state-path) 
                              (cons next-snd path)))
            (extract-path goal (cdr state-path) path)))))

(define (path start goal)
  (let* ([new-start (list (sort (first start) comp)
                          (sort (second start) comp)
                          (third start))]
         [new-goal (list (sort (first goal) comp)
                         (sort (second goal) comp)
                         (third goal))])
    (define (path-iter state-queue state-path seen-list)
      (if (null? state-queue) "The path doesn't exist"
          (let ((state (caar state-queue)))
            (if (equal? state new-goal)
                (extract-path state state-path (list state))
                (let* ((newstate-list 
                        (filter (lambda (st) 
                                  (not (member st seen-list)))
                                (possible-ways state)))
                       (newstate-pairs 
                        (map (lambda (st) (list st state)) 
                             newstate-list)))
                  (path-iter (append (cdr state-queue) newstate-pairs)
                             (append newstate-pairs state-path)
                             (append newstate-list seen-list)))))))
      (path-iter (list (list new-start '())) 
           (list (list new-start '()))
           (list new-start))))

(pretty-display (path
            '(((m . "John")
               (k . "Sasha")
               (m . "Peter")
               (k . "Vitya")
               (m . "Maria")
               (k . "Dasha"))
              ()
              L)

            '(()
              ((m . "John")
               (k . "Sasha")
               (m . "Peter")
               (k . "Vitya")
               (m . "Maria")
               (k . "Dasha"))
              R)))
;(save? '(((m . 2) (k . 2)) ((m . 3) (k . 3) (k . 4))))
;(pretty-print (all-ways '((m1 k1 m2 k2) (m3 k3) R)))
;(move '(1 2 3 4) '(5 6) '(3 4))
