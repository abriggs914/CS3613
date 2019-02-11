#lang racket

(define (my-map f lst)
  (match lst
    ['() '()]
    [(cons head tail) (cons (f head)
                            (my-map f tail))]))

(define (my-map2 f lst)
  (match lst
    ['() '()]
    [(list head tail ...) (cons (f head)
                                (my-map2 f tail))]))

;(match '(1 2 3 4)
;  [(list a b a ...) (list a b)]
;  [(list a b c ...) (list c b a)])



(define (list-length lst)
  (define (helper lst acc)
    (match lst
      ['() acc]
      [(cons a b) (helper (rest lst) (add1 acc))]))
  (helper lst 0))

;(define (list-length3 lst)
;  (match lst
;      ['() lst]
;      [(list a b ...) (add1 (list-length3 (rest lst)))]))

(define (list-length2 lst)
  (define (helper lst acc)
    (match lst
      ['() acc]
      [(list a b ...) (helper (rest lst) (add1 acc))]))
  (helper lst 0))



(module+ test
  (require rackunit)
  (check-equal? (my-map sub1 '(1 2 3)) '(0 1 2)))

(module+ test
  (require rackunit)
  (check-equal? (my-map2 sub1 '(1 2 3)) '(0 1 2)))

(module+ test
  (require rackunit)
  (check-equal? (list-length '(1 2 3)) 3)
  (check-equal? (list-length '()) 0))

(module+ test
  (require rackunit)
  (check-equal? (list-length2 '(1 2 3)) 3)
  (check-equal? (list-length2 '()) 0)
  (check-equal? (list-length2 '(1 2 3 4 5 6 7 8 9 1 0)) 11))
