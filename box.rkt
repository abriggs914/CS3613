#lang racket
(require rackunit)
(require explorer)

(define c 16)
(define (a b)
  (let ([x 1])
    (+ x b)))
(a c)
(set! c 5) ; overwrite c
(a c)

(define b (box 10))
(unbox b)
(set-box! b 20) ; overwrite b
(unbox b)
(define d (begin (+ 1 2) (- 7 8)))

(module+ test
  (check-equal? (string->number (string-append (number->string 1) (number->string 1))) 11))