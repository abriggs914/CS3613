#lang plait

(define-type WAE
  [With (id : Symbol) (bound-expr : WAE) (body : WAE)]
  [Add (left : WAE) (right : WAE)]
  [Id (id : Symbol)]
  [Num (n : Number)])

(define (find-ids expr)
  (type-case WAE expr
    [(With id exp1 exp2) (append (list id) (find-ids exp2))]
    [(Add left right) (append (find-ids left) (find-ids right))]
    [else empty]))

(test (find-ids (Num 3)) '())
(test (find-ids (Id 'x)) '())
(test (find-ids (With 'x (Num 3) (Id 'x))) '(x))
(test (find-ids (Add
                 (With 'x (Num 3) (Id 'x))
                 (With 'y (Num 3) (Id 'y)))) '(x y))
(test (find-ids (Add
                 (With 'x (Num 3) (Id 'x))
                 (With 'x (Num 3) (Id 'y)))) '(x x))


(define (alpha f g)
  (lambda (x)
    (g (f x))))

(has-type  alpha : (('a -> 'b) ('b -> 'c) -> ('a -> 'c)))

(test ((alpha add1 add1) 1) 3)
(test ((alpha add1 to-string) 42) "43")

(define-type Object
  [object (refs : (Listof Number))]
  [integer (n : Number)])

(define-type-alias Heap (Vectorof Object))
(define example-heap
  (vector
   (integer 1)          ; 0
   (object (list 8))    ; 1
   (object (list 1))    ; 2
   (object (list 7))    ; 3
   (object (list 0 3))    ; 4
   (integer 2)          ; 5
   (object (list 0))    ; 6
   (object (list 4 6)) ; 7
   (object (list 2)))) ; 8


(define pi 3.141592653589793)
(define-type Answer
  [num (n : Number)]
  [bool (b : Boolean)])

(define (shape% method)
  (case method
    [(round?) (bool #f)]
    [else (error method "unknown method")]))

(define (circle radius)
    (lambda (m) (cond
                  [(equal? m 'round?) (bool #t)]
                  [(equal? m 'area) (num (* pi radius))]
                  [else (shape% m)])))

(define (square side-length)
    (lambda (m) (if (equal? m 'area) (num (* side-length side-length)) (shape% m))))

(define a-square (square 1))
(define a-circle (circle 1))

(test (a-square 'area) (num 1))
(test (a-circle 'area) (num pi))
(test (a-square 'round?) (bool #f))
(test (a-circle 'round?) (bool #t))
(test/exn (a-circle 'perimeter?) "unknown method")


(define-type numtree
  [leaf (n : Number)]
  [tree (l : numtree) (r : numtree)])

(define (sumtree t)
  (type-case numtree t
    [(leaf n) n]
    [(tree l r) (+ (sumtree l) (sumtree r))]))

(test (sumtree (tree
                (tree (leaf 3) (leaf 4))
                (tree (leaf 5) (tree (leaf 6) (leaf 7)))))
       25)
