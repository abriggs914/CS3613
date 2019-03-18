#lang plait #:untyped

(require (typed-in racket/base
                   [number? : ('a -> Boolean)]
                   [procedure? : ('a -> Boolean)]))

(define (make-tree left value right)
  (lambda (key)
      (case key
        [(getLeft) left]
        [(getValue) value]
        [(getRight) right])))

(define empty-tree 'emptyTree)
(define (empty-tree? val) (eq? val 'emptyTree))

(test ((make-tree empty-tree 0 empty-tree) 'getLeft) empty-tree)
(test ((make-tree empty-tree 0 empty-tree) 'getRight) empty-tree)
(test ((make-tree empty-tree 0 empty-tree) 'getValue) 0)

(define (make-leaf num)
  (make-tree empty-tree num empty-tree))

(test ((make-leaf 3) 'getValue) 3)
(test ((make-leaf 5) 'getLeft) empty-tree)
(test ((make-leaf 5) 'getRight) empty-tree)


(define test-tree-1
  (make-tree (make-leaf 1) 2 (make-leaf 3)))

(define (non-empty val)
  (cond
   [(number? val) (error 'non-empty "expected non-empty tree")]
   [(eq? val empty-tree) (error 'non-empty "expected non-empty tree")]
   [else val]))


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


(define (left-child tree)
  (let ([ret (tree 'getLeft)])
    (if (number? ret) (error 'left-child "bad tree") ret)))

(test (left-child (make-leaf 3)) empty-tree)
(test/exn (left-child (lambda (x) 3))  "bad tree")
(test (left-child 
       ( non-empty (left-child test-tree-1))) empty-tree)

(define (right-child tree)
  (let ([ret (tree 'getRight)])
    (if (number? ret) (error 'right-child "bad tree") ret)))

(test (right-child (make-leaf 3)) empty-tree)
(test/exn (right-child (lambda (x) 3)) "bad tree")
      
(test (right-child 
       ( non-empty (left-child test-tree-1))) empty-tree)

(define (value tree)
  (let ([ret (tree 'getValue)])
    (if (number? ret) ret (error 'value "bad tree"))))
(test/exn (value (lambda (x) empty-tree)) "bad tree")
(test (value 
       ( non-empty (left-child test-tree-1))) 1)


(define (tree-sum tree)
  (let ([left (left-child tree)]
        [right (right-child tree)]
        [the-value (value tree)])
    (+ the-value 
       (if (empty-tree? left)
           0
           (tree-sum left))
       (if (empty-tree? right)
           0
           (tree-sum right)))))
   
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


(define (lift fun)
  (lambda (tree)
    (tree-map fun tree)))

(define tree-add1 (lift add1))
(define tree-neg (lift (lambda (x) (* -1 x))))

(test (tree-sum (tree-add1 test-tree-1)) 9)
(test (tree-sum (tree-neg test-tree-1)) -6)
#|
There is no reason for this function to be specific to trees, 
It works for any two functions of types ('b -> 'c) and ('a -> 'b)
|#
(define (compose fun1 fun2)
  (lambda (tree)
    (fun1 (fun2 tree))))


(test (tree-sum ((compose tree-neg tree-add1) test-tree-1)) -9)
(test (tree-sum ((compose tree-add1 tree-neg) test-tree-1)) -3)
(test (tree-sum ((compose tree-add1 tree-add1) test-tree-2)) 18)
(test (tree-sum ((compose tree-neg tree-neg) test-tree-2)) (tree-sum test-tree-2))

(define minutes-spent 120)