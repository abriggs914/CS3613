#lang plait #:untyped

#|
  CS3613 Homework 7
  Mar.15/19
  Avery Briggs
  3471065
|#

(define (make-tree left value right)
  (lambda (key)
    (let* ([lst (list (symbol->s-exp key))]
           [key (list-ref lst 0)])
      (cond
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getLeft)) 0)) left]
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getValue)) 0)) value]
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getRight)) 0)) right]
        [else (error 'make-tree "Input error")]))))

(define empty-tree empty)
(define (empty-tree? val) (if (empty? val) #t #f))

(test ((make-tree empty-tree 1 empty-tree) 'getLeft) empty-tree)
(test ((make-tree empty-tree 2 empty-tree) 'getRight) empty-tree)
(test ((make-tree empty-tree 3 empty-tree) 'getValue) 3)

(define (make-leaf val)
  (lambda (x) ((make-tree empty-tree val empty-tree) x)))

(test ((make-leaf 3) 'getValue) 3)
(test ((make-leaf 5) 'getLeft) empty-tree)
(test ((make-leaf 5) 'getRight) empty-tree)

(require (typed-in racket/base
                   [number? : ('a -> Boolean)]
                   [procedure? : ('a -> Boolean)]))

(define (non-empty tree)
  (cond
    [(procedure? tree) tree]
    [else (error 'non-empty "expected non-empty tree")]))

(define (right-child tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree]
    [(non-empty tree) 
     (if (number? (tree 'getRight)) 
         (error 'right-child "bad tree") 
         (tree 'getRight))]
    [else (error 'right-child "bad tree")]))

(define (left-child tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree]
    [(non-empty tree) 
     (if (number? (tree 'getLeft)) 
         (error 'left-child "bad tree") 
         (tree 'getLeft))]
    [else (error 'left-child "bad tree")]))

(define (value tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree]
    [(non-empty tree) 
     (if (number? (tree 'getValue)) 
         (tree 'getValue)
         (error 'value "bad tree"))]
    [else (error 'value "bad tree")]))

(define (tree-sum tree)
  (let ([sum 0])  
    (local
      [(define (helper ltree mid rtree acc)
       (cond
         [(equal? empty-tree ltree) acc]
         [(equal? empty-tree rtree) acc]
         [(equal? empty-tree mid) acc]
         [else (helper (ltree 'getLeft) mid (rtree 'getRight) (+ acc (ltree 'getValue) (mid 'getValue) (rtree 'getValue)))]))]
      (helper (tree 'getLeft) tree (tree 'getRight) sum))))

(define (lift fun)
  (lambda (x)
    (local
      [(define (helper fun l m r)
         (let
             ([getL (l 'getLeft)]
              [getR (r 'getRight)])
         (cond
           [(equal? empty-tree getL) x]
           [(equal? empty-tree getR) x]
           [(equal? empty-tree m) x]
           [else (helper fun (fun getL) m (fun getR))])))]
      (helper fun (x 'getLeft) (fun (x 'getValue)) (x 'getRight)))))

(define test-tree-1
  (make-tree (make-leaf 1) 2 (make-leaf 3)))
(test (non-empty test-tree-1) test-tree-1)
(test/exn (non-empty empty-tree) "expected non-empty tree")
(test/exn (non-empty 42)  "expected non-empty tree")
(test (left-child (make-leaf 3)) empty-tree)
(test/exn (left-child (lambda (x) 3))  "bad tree")
(test/exn (value (lambda (x) empty-tree)) "bad tree")
(test (value (non-empty (left-child test-tree-1))) 1)

(test (tree-sum test-tree-1) 6)

(define tree-add1 (lift add1))
(test (tree-sum (tree-add1 test-tree-1)) 9)

;(define tree-neg (lift (lambda (x) (* -1 x))))
;(test (tree-sum ((compose tree-neg tree-add1) test-tree-1)) -9)
;(test (tree-sum ((compose tree-add1 tree-neg) test-tree-1)) -3)

(define minutes-spent 120)