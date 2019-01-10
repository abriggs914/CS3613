#lang plait

(define pi 3.141592653589793)

(define-type Shape
  [Square  (side : Number)]
  [Circle  (radius : Number)]
  [Triangle (height : Number) (width :  Number)])

#|
(define (area [shape : Shape]) : Number
;; fill in here
  (cond
    [(Square? shape) (type-case Shape shape [(Square s) (* s s)] [else 0])]
    [(Circle? shape) (* (type-case Shape shape [(Circle r) (* r r)] [else 0]) pi)]
    [(Triangle? shape) (type-case Shape shape [(Triangle h w) (* h (* 1/2 w))] [else 0])]))
|#


(define (area [shape : Shape]) : Number
     (type-case Shape shape
       [(Square s) (* s s)] ;; s^2
       [(Circle r) (* pi (* r r))] ;; pi*r^2
       [(Triangle h w) (* h (* 1/2 w))])) ;; 1/2*b*h


(define tri (Triangle 10 12))
(define squ (Square 12))
(define cir (Circle 15))
;(define x (area tri))
;(define y (area squ))
;(define z (area cir))

(test (area tri) 60)
(test (area squ) 144)
(test (area cir) 706.8583470577034)


