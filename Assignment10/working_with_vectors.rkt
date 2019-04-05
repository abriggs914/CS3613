#lang plait
(define-syntax-rule (set x ...)  
  (list->set (list x ...)))

(define-type-alias (Setof 'a) (Hashof 'a Boolean))

(define (list->set [ items : (Listof 'a) ]) : (Setof 'a)
  (hash (map (lambda (item) (pair item #t)) items)))

(define a (set 1 2 3))

(define b (vector 1 2 3 4 5))

(define (vector->list v)
  (let ([m (vector-length v)]
        [f (vector-ref v 0)])
  (local [(define (helper acc c)
           (cond
             [(= c m) acc]
             [else (helper (append acc (list (vector-ref v c))) (add1 c))]))]
    (helper empty 0))))

(vector->list b)
 ;------------------------------------------------------------
#lang plait

#|
 CS3613 Homework 10
 Fri Apr. 5 /19
 Avery Briggs
 3471065
|#

(print-only-errors #t)

(define-type Object
  [object (refs : (Listof Number))]
  [primitive])

;(define-type Heap
;  [heap (heap : (Vectorof Object))]
;  [p (primitive : Object)])

(define-type-alias Heap (Vectorof Object))
(define-type-alias LocSet (Setof Number))
(define-type-alias (Setof 'a) (Hashof 'a Boolean))

(define (list->set [ items : (Listof 'a) ]) : (Setof 'a)
  (hash (map (lambda (item) (pair item #t)) items)))

(define S123 (list->set '(1 2 3)))

(define-syntax-rule (set x ...)  
  (list->set (list x ...)))

(define (set-member? [set : (Setof 'a)]  [thing : 'a]) : Boolean
  (type-case (Optionof Boolean) (hash-ref set thing)
    [(some val) val]
    [else #f]))

(define (set-add [set : (Setof 'a)] [thing : 'a]) : (Setof 'a)
  (hash-set set thing #t))

(define (set-union [the-set : (Setof 'a)] [other-set : (Setof 'a)])    
  (foldl
   (lambda (thing set) (set-add set thing))
   the-set 
   (hash-keys other-set)))


(module+ test
  (test (list->set empty)  (hash empty))
  (test  S123 (hash (list (pair 1 #t) (pair 2 #t) (pair 3 #t))))
  (test (set-union (set 0 1 2 3) (set -1 2 7)) (set -1 0 1 2 3 7))
  (test (set) (list->set empty))
  (test (set 1 2 3) S123)
  (test (set-member? S123 1) #t)
  (test (set-member? S123 10) #f)
  (test (set-add (set 1 2 3) 0) (set 0 1 2 3))
  )

(define (search-one-root [n : Number] [h : Heap]) : LocSet
;  ; typecase object
  (let
      ([s (set)]
       [lst (vector->list h)])
;    (set lst)))
    (set-union s
               (list->set
                (foldl
                 (lambda (x acc)
                   (type-case Object x
                     [(primitive) acc]
                     [(object ref) (append acc ref)])) (list n) lst)))))

;(local [(define (helper lst)
    ;          ())])
   ;         (helper (list h) 0)])
    ;(set-add s n)))


(define (vector->list v)
  (let ([m (vector-length v)])
    (local [(define (helper acc c)
              (cond
                [(= c m) acc]
                [else (helper (append acc (list (vector-ref v c))) (add1 c))]))]
      (helper empty 0))))

;(has-type search-one-root : (Number Heap -> LocSet))

(define heap1
  (vector
   (object (list 1 2))
   (primitive)
   (object (list 2))))

(define heap2
  (vector
   (object (list 0 1))
   (object (list 2))
   (object (list 0))))


(module+ test
(test (search-one-root 0 heap1) (set 0 1 2))
(test (search-one-root 1 heap1) (set 1))
(test (search-one-root 0 heap2) (set 0 1 2))
(test (search-one-root 1 heap2) (set 0 1 2)))