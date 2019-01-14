#lang plait

#|
CS 3613
Homework 1
Avery Briggs
3471065
|#


;; Question 1

(define pi 3.141592653589793)

(define-type Shape
  [Square  (side : Number)]
  [Circle  (radius : Number)]
  [Triangle (height : Number) (width :  Number)])

;;(Shape -> Number)
(define (area [shape : Shape]) : Number
  (type-case Shape shape
    [(Square s) (* s s)] ;; s^2
    [(Circle r) (* pi (* r r))] ;; pi*r^2
    [(Triangle h w) (* h (* 1/2 w))])) ;; 1/2*b*h

;; Question 2

;;(Number Number Number -> Number)
(define (pow acc base exp)
  (cond
    [(zero? exp) acc]
    [else (pow (* acc base) base (- exp 1))]))

;;(Number Number (Listof Number) -> Number)
(define (helper num tot lst)
  (cond
    [(empty? lst) tot]
    [else (helper (+ 1 num) (+ (* (first lst) (pow 1 3 num)) tot) (rest lst))]))

;;((Listof Number) -> Number)
(define (tern->num numbers)
  (helper 0 0 numbers))

;; Question 3

(define minutes-spent 90)

;; Testing

;; Question 1
(define tri (Triangle 10 12))
(define squ (Square 12))
(define cir (Circle 15))

(test (area tri) 60)
(test (area squ) 144)
(test (area cir) 706.8583470577034)

;; Question 2
(test (tern->num  empty) 0)
(test (tern->num (list 1)) 1)
(test (tern->num (list 2)) 2)
(test (tern->num (list 1 0 2 1)) 46)
(test (tern->num (list 1 2 0 1)) 34)
(test (tern->num (list 1 2 0 1 2 1)) 439)