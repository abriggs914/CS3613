#lang plai
(require plai/dynamic)

; dynamic typing allows for variables to be
; declared globally, despite being declared
; locally

(define x 123)
(define (getx) x)

(define (bar1 x) (getx))
(define (bar2 y) (getx))

(test (getx) 123)
(test (let ([x 456]) (getx)) 456)
(test (getx) 123)
(test (bar1 999) 999)
(test (bar2 999) 123)