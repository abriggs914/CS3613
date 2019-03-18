#lang plait #:untyped

#|
  CS3613 Homework 7
  Mar.15/19
  Avery Briggs
  3471065
|#


(require (typed-in racket/base
                   [number? : ('a -> Boolean)]
                   [procedure? : ('a -> Boolean)]))

;; returns a function that creates a binary tree structure.
;; can be called with one of 'getLeft 'getRight 'getValue,
;; which will return a node.
(define (make-tree left value right)
  (lambda (key)
    (let* ([lst (list (symbol->s-exp key))]
           [key (list-ref lst 0)])
      (cond
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getLeft)) 0)) left]
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getValue)) 0)) value]
        [(s-exp-match? key (list-ref (list (symbol->s-exp 'getRight)) 0)) right]))))

(define empty-tree empty)
(define (empty-tree? val) (if (empty? val) #t #f))

;; returns a new leaf node for a binary tree.
(define (make-leaf val)
  (lambda (x) ((make-tree empty-tree val empty-tree) x)))

;; verifies that a given tree is not empty, returns the tree.
(define (non-empty tree)
  (cond
    [(procedure? tree) tree]
    [else (error 'non-empty "expected non-empty tree")]))

;; returns the node value of the right child of a given tree.
(define (right-child tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree]
    [(non-empty tree) 
     (if (number? (tree 'getRight)) 
         (error 'right-child "bad tree") 
         (tree 'getRight))]))

;; returns the node value of the left child of a given tree.
(define (left-child tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree] 
    [(non-empty tree) 
     (if (number? (tree 'getLeft)) 
         (error 'left-child "bad tree") 
         (tree 'getLeft))]))

;; returns the value of the current node of the tree.
(define (value tree)
  (cond
    [(number? tree) tree]
    [(empty-tree? tree) empty-tree] 
    [(non-empty tree) 
     (if (number? (tree 'getValue)) 
         (tree 'getValue)
         (error 'value "bad tree"))]))

;; sums each node in the tree, returning the result.
(define (tree-sum tree)
  (let ([sum 0])
    (local
      [(define (helper t acc)
         (cond
           [(equal? empty-tree t) acc]
           [else (+ acc (helper (t 'getLeft) acc)
                    (t 'getValue)
                    (helper (t 'getRight) acc))]))]
      (helper tree sum))))

;; applies a given function to a tree structure and returns it.
;; function must be of the type (Number -> Number) since all
;; stored nodes are numbers.
(define (lift fun)
  (lambda (x)
    (local
      [(define (helper t fun)
         (cond
           [(equal? empty-tree t) empty-tree]
           [else (make-tree
                  (helper (t 'getLeft) fun)                    
                  (fun (t 'getValue))
                  (helper (t 'getRight) fun))]))]
      (helper x fun))))

;; takes in two functions and applies lift to the calling
;; tree structure in order for both functions.
;; functions must be of the type (Number -> Number) since all
;; stored nodes are numbers.
(define (compose funA funB)
  (let
      ([funA (lambda (x) (funA x))]
       [funB (lambda (x) (funB x))])
    (lambda (x) (funA (funB x)))))


(test ((make-tree empty-tree 0 empty-tree) 'getLeft) empty-tree)
(test ((make-tree empty-tree 0 empty-tree) 'getRight) empty-tree)
(test ((make-tree empty-tree 0 empty-tree) 'getValue) 0)

(test ((make-leaf 3) 'getValue) 3)
(test ((make-leaf 5) 'getLeft) empty-tree)
(test ((make-leaf 5) 'getRight) empty-tree)

(define test-tree-1
  (make-tree (make-leaf 1) 2 (make-leaf 3)))


;; Unbalanced example
(define test-tree-2
  (make-tree empty-tree 1
             (make-tree empty-tree 2
                        (make-tree empty-tree 3
                                   (make-leaf 4)))))

(test/exn (non-empty empty-tree) "expected non-empty tree")
(test/exn (non-empty 42)  "expected non-empty tree")
(test (non-empty test-tree-1) test-tree-1)
(test (non-empty test-tree-2) test-tree-2)

(test (left-child (make-leaf 3)) empty-tree)
(test/exn (left-child (lambda (x) 3))  "bad tree")
(test (left-child 
       ( non-empty (left-child test-tree-1))) empty-tree)

(test (right-child (make-leaf 3)) empty-tree)
(test/exn (right-child (lambda (x) 3)) "bad tree")
      
(test (right-child 
       ( non-empty (left-child test-tree-1))) empty-tree)

(test/exn (value (lambda (x) empty-tree)) "bad tree")
(test (value 
       ( non-empty (left-child test-tree-1))) 1)

(test (tree-sum test-tree-1) 6)
(test (tree-sum test-tree-2) 10)

(define (tree-map fun tree)
  (let ([left (left-child tree)]
        [right (right-child tree)]
        [the-value (value tree)])
    (make-tree
     (if (empty-tree? left)
         left
         (tree-map fun left))
     (fun the-value)
     (if (empty-tree? right)
         right
         (tree-map fun right)))))

(test (tree-sum (tree-map add1 test-tree-1)) 9)
(test (tree-sum (tree-map add1 test-tree-2)) 14)

(define tree-add1 (lift add1))
(define tree-neg (lift (lambda (x) (* -1 x))))

(test (tree-sum (tree-add1 test-tree-1)) 9)
(test (tree-sum (tree-neg test-tree-1)) -6)

(test (tree-sum ((compose tree-neg tree-add1) test-tree-1)) -9)
(test (tree-sum ((compose tree-add1 tree-neg) test-tree-1)) -3)
(test (tree-sum ((compose tree-add1 tree-add1) test-tree-2)) 18)
(test (tree-sum ((compose tree-neg tree-neg) test-tree-2)) (tree-sum test-tree-2))

(define minutes-spent 240)