#lang plait

#|
 CS3613 Homework 10
 Fri Apr. 5 /19
 Avery Briggs
 3471065
|#

(define-type Object
  [object (refs : (Listof Number))]
  [primitive])

(define-type-alias Heap (Vectorof Object))
(define-type-alias LocSet (Setof Number))
(define-type-alias (Setof 'a) (Hashof 'a Boolean))

(define (list->set [ items : (Listof 'a) ]) : (Setof 'a)
  (hash (map (lambda (item) (pair item #t)) items)))

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

(define (visitObject [n : Number] [h : Heap] [acc : (Listof Number)]) : (Listof Number)
  (let ([len (vector-length h)])
    (cond
      [(>= n len)
       (error 'visitObject "Given an index greater than vector length.")]
      [(< n 0)
       (error 'visitObject "Given a negative index value.")]
      [else (type-case Object (vector-ref h n)
              [(primitive) acc]
              [(object ref) ref])])))

(define (search-one-root [n : Number] [h : Heap]) : LocSet
  (let ([lst (visitObject n h (list n))]
         [e empty])
    (list->set
     (append (list n)
             (append lst (foldl
                          (lambda (x acc)
                            (visitObject x h (append acc e)))
                          e
                          (visitObject n h e)))))))

(define (find-live [lst : (Listof Number)] [h : Heap]) : LocSet
  (let ([res
          (append lst
                  (foldl
                   (lambda (x acc)
                     (append acc (visitObject x h empty)))
                   empty lst))])
    (list->set res)))

;Testing

(has-type search-one-root : (Number Heap -> LocSet))
(has-type find-live : ((Listof Number) Heap -> LocSet))

(define S123 (list->set '(1 2 3)))

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

(define heap3
  (vector
   (object (list 1 5 6 7 8)) ;0
   (object (list 0)) ;1
   (primitive) ;2
   (primitive) ;3
   (object (list 0 1)) ;4
   (object (list 5 6)) ;5
   (primitive) ;6
   (object (list 0 2 4)) ;7
   (object (list 2)) ;8
   (object (list 1 2 3)))) ;9

(define heap4
  (vector
   (primitive)))

(define heap5
  (vector
   (object (list 2))
   (object (list 2 3))
   (primitive)
   (object (list 4))
   (primitive)))


(module+ test
  (print-only-errors #t)
  (test (list->set empty)
        (hash empty))
  (test  S123
         (hash (list (pair 1 #t) (pair 2 #t) (pair 3 #t))))
  (test (set-union (set 0 1 2 3) (set -1 2 7))
        (set -1 0 1 2 3 7))
  (test (set)
        (list->set empty))
  (test (set 1 2 3)
        S123)
  (test (set-member? S123 1)
        #t)
  (test (set-member? S123 10)
        #f)
  (test (set-add (set 1 2 3) 0)
        (set 0 1 2 3))

  ;; Since object refs correspond to indexes, only
  ;; positive integers may be given as param n in
  ;; search-one-root and param lst in find-live. Also,
  ;; this means that all heaps with lists of object refs
  ;; will only be positive integers as well.
  (test (search-one-root 0 heap1)
        (set 0 1 2))
  (test (search-one-root 1 heap1)
        (set 1))
  
  (test (search-one-root 0 heap2)
        (set 0 1 2))
  (test (search-one-root 1 heap2)
        (set 0 1 2))
  
  (test (search-one-root 0 heap3)
        (set 0 1 2 5 6 7 8))
  (test (search-one-root 1 heap3)
        (set 0 1 5 6 7 8))
  
  (test (search-one-root 0 heap4)
        (set 0))
  
  (test (search-one-root 1 heap5)
        (set 1 2 3 4))
  (test (search-one-root 0 heap5)
        (set 0 2))
  
  (test/exn (search-one-root 6 heap1)
            "visitObject: Given an index greater than vector length.")
  (test/exn (search-one-root -1 heap3)
            "visitObject: Given a negative index value.")
  
  (test (find-live '(1) heap1)
        (set 1))
  (test (find-live '(0) heap1)
        (set 0 1 2))
  
  (test (find-live '(1 2) heap2)
        (set 0 1 2))
  
  (test (find-live '(0 1 3) heap3)
        (set 0 1 3 5 6 7 8))
  
  (test (search-one-root 0 heap4)
        (set 0))
  
  (test (search-one-root 1 heap5)
        (set 1 2 3 4))
  (test (search-one-root 0 heap5)
        (set 0 2))
  
  (test/exn (find-live '(6) heap1)
            "visitObject: Given an index greater than vector length.")
  (test/exn (find-live '(-1) heap3)
            "visitObject: Given a negative index value."))

(define minutes-spent 240)