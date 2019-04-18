#lang plait

(define-type wrapper
  [num (n : Number)]
  [str (s : String)])

(define (a x)
  (if x (num 42) (str "oops")))

; kind of like a list
(pair 1 (pair 0 (pair 1 (pair 0 2))))