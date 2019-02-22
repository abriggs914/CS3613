#lang plait

(define-type SHAPE
  [circle (r : Number)]
  [triangle (b : Number) (h : Number)]
  [rectangle (l : Number) (w : Number)])

(define cir (circle 15))
(define tri (triangle 4 10))
(define rec (rectangle 10 12))

(define (area shape)
  (type-case SHAPE shape
  [(circle r) (* 3.1415 (* r r))]
  [(triangle b h) (* (* 0.5 b) h)]
  [(rectangle l w) (* l w)]))

(define a (area cir))
(define b (area tri))
(define c (area rec))

(define (f a b)
  (* a b))

(define (g a lst)
  (foldl (lambda (x y) (+ y x)) a lst))
(has-type f : ('a 'b -> 'c))

(define (eval sx)
  (let ([rec (lambda (fn)
              (eval (fn (s-exp->list sx))))])
    (cond
      [(s-exp-match? `NUMBER sx)
       (s-exp->number sx)]
      [(s-exp-match? `(+ ANY ANY) sx)
       (+ (rec second) (rec third))]
      [(s-exp-match? `(- ANY ANY) sx)
       (- (rec second) (rec third))]
      [else (error 'eval (to-string sx))])))

(module+ test
  (test (eval `(+ 1 2)) 3)
  (test (eval `(- 8 (+ 1 2))) 5)
  (test/exn (eval `{+ 1 {- 3 "a"}}) "eval: `\"a\""))