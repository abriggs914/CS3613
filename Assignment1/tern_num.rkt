#lang plait
;(require "math.rkt")

(define (hello n)
  (+ n 4))

;else (let [(x (+ 1 num))] (helper x (+ (* (first lst) (pow 1 3 num)) t) (rest lst)))

(define (pow acc b e)
  (cond
    [(zero? e) acc]
    [else (pow (* acc b) b (- e 1))]))

(define (helper num t lst)
  (cond
    [(empty? lst) t]
    [else (let [(x (+ 1 num))]
            (helper x (+ (* (first lst) (pow 1 3 num)) t) (rest lst)))]))

(define (tern->num numbers)  
  (helper 0 0 numbers))

;;((Listof Number) -> Number)


(test (tern->num  empty) 0)
(test (tern->num  (list 1)) 1)
(test (tern->num  (list 2)) 2)
(test (tern->num (list 1 0 2 1)) 46)
(test (tern->num (list 1 2 0 1)) 34)


(define ab (tern->num (list 1 2 0 1 2 1)))