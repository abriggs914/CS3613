#lang plait

(define (f x y)
 (* x y))

(define (fP x y)
  ((lambda (a b) (* a b)) x y))

(define (f2 x y)
  (if
   (or (= x 0) (= y 0)) 0
   (local
     [(define (helper x y acc)
        (cond
          [(= y 0) acc]
          [else (helper x (sub1 y) (+ x acc))]))]
     (helper x y 0))))

(test (f 5 6) (fP 5 6))
(test (f 5 6) (f2 5 6))
(test (f2 5 6) (fP 5 6))
(test (f 0 19) 0)
(test (fP 0 19) 0)
(test (f2 0 19) 0)
(test (f2 19 0) 0)

(define (currify f)
  (lambda (x) (lambda (y) (f x y))))

(define (plus)
  (currify +))

(test (((plus) (((plus) (((plus) (((plus) 1) 2)) 10)) -85)) 14) -58)

(foldl (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
;- (Listof String)

;'("3" "2" "1")

(foldr (lambda (n r) (cons (to-string n) r)) empty (list 1 2 3))
;- (Listof String)

;'("1" "2" "3")
;(cons 4 '("string"))