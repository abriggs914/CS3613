#lang plait

;; Solution for tutorial 7
(define-type-alias (Setof 'a) (Hashof 'a Boolean))
(define (list->set [ items : (Listof 'a) ]) : (Setof 'a)
  (hash (map (lambda (x) (pair x #t)) items)))
(define-syntax-rule (set items ...)
  (list->set (list items ...)))
(define (set-member? [set : (Setof 'a)]  [thing : 'a]) : Boolean
  (equal? (hash-ref set thing) (some #t)))
(define (set-add [set : (Setof 'a)] [thing : 'a]) : (Setof 'a)
  (hash-set set thing #t))
(define (set-union [the-set : (Setof 'a)] [other-set : (Setof 'a)])
  (hash (map (lambda (x) (pair x #t))
             (append (hash-keys the-set) (hash-keys other-set)))))

(module+ test
  (test (list->set empty)  (hash empty))
  (define S123 (list->set '(1 2 3)))
  (test  S123 (hash (list (pair 1 #t) (pair 2 #t) (pair 3 #t))))
  (test (set) (list->set empty))
  (test (set 1 2 3) S123)
  (test (set-member? S123 1) #t)
  (test (set-member? S123 10) #f)
  (test (set-add (set 1 2 3) 0) (set 0 1 2 3))
  (test (set-union (set 0 1 2 3) (set -1 2 7)) (set -1 0 1 2 3 7)))

(define-type Object
  [object (refs : (Listof Number))]
  [primitive])

(define-type-alias Heap (Vectorof Object))
(define-type-alias LocSet (Setof Number))

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

(define (search-one-root [root : Number] [heap : Heap])
  (search-one-root-unknown root heap (set)))

(define (search-one-root-unknown [root : Number] [heap :  Heap] [known : LocSet]) : LocSet
  (if (set-member? known root)
      known
      (type-case Object (vector-ref heap root)
        [(object refs)  (find-live-unknown refs heap (set-add known root))]
        [(primitive) (set-add known root)])))

(define (find-live-unknown [roots : (Listof Number)] [heap : Heap] [known : LocSet]) : LocSet
  (if
   (empty? roots)
   known
   (set-union
    (search-one-root-unknown (first roots) heap known)
    (find-live-unknown (rest roots) heap (set-add known (first roots))))))

(module+ test
  (test (search-one-root 0 heap1)  (set 0 1 2))
  (test (search-one-root 1 heap1)  (set 1))

  (test (search-one-root 0 heap2)  (set 0 1 2))
  (test (search-one-root 1 heap2)  (set 0 1 2)))


(define (find-live [roots : (Listof Number)] [heap : Heap]) : LocSet
  (find-live-unknown roots heap (set)))

(module+ test
  (test (find-live '(1) heap1)  (set 1))
  (test (find-live '(0) heap1)  (set 0 1 2))
  (test (find-live '(1 2) heap2)  (set 0 1 2))

  (define heap4
    (vector
     (object (list 3))
     (object (list 3 4))
     (object (list 4 5))
     (object (list 0))
     (primitive)
     (primitive)))

  (test (search-one-root 1 heap4)  (set 0 1 3 4))
  (test (search-one-root 2 heap4)  (set 2 4 5))
  (test (search-one-root 3 heap4)  (set 0 3))
  (test (search-one-root 5 heap4)  (set 5))

  (test (find-live '(1) heap4)  (set 0 1 3 4))
  (test (find-live '(2) heap4)  (set 2 4 5))
  (test (find-live '(1 2) heap4)  (set 0 1 2 3 4 5))
  (test (find-live '(3 4) heap4)  (set 0 3 4)))

(define minutes-spent 120)