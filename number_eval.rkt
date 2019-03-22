#lang racket

(define (eval a)
  (if a "true" "false"))

(module+ test
  (require rackunit)
  (check-equal? (eval (= 0 0.0)) "true")
  (check-equal? (eval #f) "false")
  (check-equal? (eval -1) "true")
  (check-equal? (eval 1) "true"))