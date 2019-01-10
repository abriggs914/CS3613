#lang plait

;(require "math.rkt")

(define (tern->num lst)
  (define (helper num t lst)
    (cond
      [(empty? lst) t]
      [else (helper (add1 num) (+ (* (first lst) (expt 3 num)) t) (rest lst))]))
  (helper 0 0 lst))

;;((Listof Number) -> Number)


(test (tern->num  empty) 0)
(test (tern->num  (list 1)) 1)
(test (tern->num  (list 2)) 2)
(test (tern->num (list 1 0 2 1)) 46)
(test (tern->num (list 1 2 0 1)) 34)