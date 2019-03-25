#lang plait

#|
 CS3613 Tutorial 7
 Mon Mar.25/19
 Avery Briggs
 3471065

 Working with lists in a hash-table structure.
|#

(define-type-alias (Setof 'a) (Hashof 'a Boolean))

(define (list->set [ items : (Listof 'a) ]) : (Setof 'a)
  (hash (map (lambda (item) (pair item #t)) items)))

(define S123 (list->set '(1 2 3)))

(module+ test
  (test (list->set empty)  (hash empty))
  (test  S123 (hash (list (pair 1 #t) (pair 2 #t) (pair 3 #t)))))

(define-syntax-rule (set x ...)  
  (list->set (list x ...)))

(module+ test
  (test (set) (list->set empty))
  (test (set 1 2 3) S123))


(define (set-member? [set : (Setof 'a)]  [thing : 'a]) : Boolean
(type-case (Optionof Boolean) (hash-ref set thing)
  [(some val) val]
  [else #f]))

(module+ test
  (test (set-member? S123 1) #t)
  (test (set-member? S123 10) #f))

(define (set-add [set : (Setof 'a)] [thing : 'a]) : (Setof 'a)
  (hash-set set thing #t))

(module+ test
  (test (set-add (set 1 2 3) 0) (set 0 1 2 3)))

(define (set-union [the-set : (Setof 'a)] [other-set : (Setof 'a)])    
  (foldl
   (lambda (thing set) (set-add set thing))
   the-set 
   (hash-keys other-set)))

(module+ test
  (test (set-union (set 0 1 2 3) (set -1 2 7)) (set -1 0 1 2 3 7)))
