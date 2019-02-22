#lang plait #:untyped

(foldl (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
;- (Listof String)

;'("3" "2" "1")

(foldr (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
;- (Listof String)

;'("1" "2" "3")
(cons 4 '("string"))

;#lang plait #:untyped
;(define (_cons x y)
;  (lambda (b)
;    (if b x y)))
;(define (_first x) (x #t))
;(define (_rest x) (x #f))
;(define a (_cons 1 'alpha))
;(define b (_cons 'beta 4))

; Selector function
(test ((lambda (x y) x) 1 2) 1)

(define (_cons x y) (lambda (s) (s x y)))
(define (_first x) (x (lambda (x y) x)))
(define (_rest x) (x (lambda (x y) y)))
(define a (_cons 1 'alpha))
(define b (_cons 'beta 4))

(test (_first a) 1)
(test (_rest b) 4)
(test (_rest a) 'alpha)
(test (_first a) 1)
(test (_rest b) 4)
(test (_rest a) 'alpha)