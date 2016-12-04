#lang racket

;; составление состояния
;; состояние составляется в форме (farmer wolf goat cabbage)
;; где farmer - берег, на котором находится феремер (e или w)
;; где wolf - берег, на котором находится волк (e или w)
;; где goat - берег, на котором находится коза (e или w)
;; где cabbage - берег, на котором находится капуста (e или w)
(define (make-state farmer wolf goat cabbage) 
  (list farmer wolf goat cabbage))

;; набор функций, для определения берега на котором находится
;; соответствующее действующее лицо в состоянии state  
;; (результат - e или w)
(define (farmer-side state) (car state))
(define (wolf-side state) (cadr state))
(define (goat-side state) (caddr state))
(define (cabbage-side state) (cadddr state))

;; проверка на то, что состояние state является безопасным
;; (никто никого и ничего не съест)
;; результат - state, если state безопасно
;; или ложь - если state небезопасно                
(define (safe state)
  (if (not (eq? (goat-side state) (farmer-side state)))
      (cond
        ((eq? (wolf-side state) (goat-side state)) #f)
        ((eq? (cabbage-side state) (goat-side state)) #f)
        (else state))
      state))

;; переход из состояния state в состояние, когда фермер один
;; переправляется на другой берег
(define (farmer-takes-self state)
  (safe (make-state 
         (opposite (farmer-side state)) 
         (wolf-side state) 
         (goat-side state) 
         (cabbage-side state))))	

;; переход из состояния state в состояние, когда фермер с волком
;; переправляется на другой берег
;; если фермер и волк на разных берегах в state, то результат ложь
(define (farmer-takes-wolf state)
  (cond
    ((equal? (farmer-side state) (wolf-side state)) 
     (safe (make-state (opposite (farmer-side state))
                       (opposite (wolf-side state)) 
                       (goat-side state) 
                       (cabbage-side state))))
    (else #f)))           

;; переход из состояния state в состояние, когда фермер с козой
;; переправляется на другой берег
;; если фермер и коза на разных берегах в state, то результат ложь
(define (farmer-takes-goat state)
  (cond
    ((equal? (farmer-side state) (goat-side state)) 
     (safe (make-state (opposite (farmer-side state))
                       (wolf-side state) 
                       (opposite (goat-side state)) 
                       (cabbage-side state))))
    (else #f)))    

;; переход из состояния state в состояние, когда фермер с капустой
;; переправляется на другой берег
;; если фермер и капуста на разных берегах в state, то результат ложь
(define (farmer-takes-cabbage state)
  (cond
    ((equal? (farmer-side state) (cabbage-side state)) 
     (safe (make-state (opposite (farmer-side state)) 
                       (wolf-side state) 
                       (goat-side state) 
                       (opposite (cabbage-side state)))))
    (else #f)))  

;; функция генерирует список состояний, в которые можно перейти из 
;; состояния state
(define (possible-ways state)
  (filter identity
          (list (farmer-takes-self state) 
                (farmer-takes-wolf state)
                (farmer-takes-goat state)
                (farmer-takes-cabbage state))))

;; функция выдает противоположный берег для берега side
;; выдает (e или w)
(define (opposite side)
  (cond
    ((eq? side 'e) 'w)
    ((eq? side 'w) 'e)))    

;; функция извлечения пути от пункта goal до начала пути 
;; из списка пар вида (пункт1 пункт2), 
;; где пункт2 - пункт из которого попали пункт1
;; path - сформированная ранее часть пути от goal до конца пути
;; предполагается, что state-path - спсиок сформированный при обходе пунктов
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

;; основная функция - поиск пути от начального состояния state
;; к конечному состоянию goal обходом в ширину
;; если путь не существует, то результат - соответствующее сообщение
(define (path start goal)
  (define (path-iter state-queue state-path seen-list)
    (if (null? state-queue) "The path doesn't exist"
        (let ((state (caar state-queue)))
          (if (equal? state goal) 
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
  (path-iter (list (list start '())) 
             (list (list start '()))
             (list start)))

;; пример запуска         
(print (path '(w w w w) '(e e e e)))