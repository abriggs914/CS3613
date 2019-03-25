#lang typed/racket
(define S (list->set '(1 2 3)))
(define S* (set 1 2 3))
(equal? S S*)
(set-member? S 1)
(define T (set-add S 4))
(set-union S T)