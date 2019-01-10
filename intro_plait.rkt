#lang plait

(define (f a)
  (cond
    [(zero? a) (+ a 1)]
    [(eq? a 1) 2]))

(test (f 1)  2)
(test (f 0)  1)
