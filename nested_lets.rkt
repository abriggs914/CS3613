#lang plait
(define (double a)
  (let ([x a])
    (let ([y x])
      (let ([x x])
        (+ x y)))))

(test (double 1) 2)
(test (double 10) 20)