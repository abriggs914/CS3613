#lang plait

;; list of strings
(define listString (list "List" "of" "String"))
;; list of functions 
(define listfun (list (lambda (x) (+ x 10)) (lambda (x) (- x 10))))

(define p (pair 1 (lambda (x) (+ x 10))))
(test ((snd p) (fst p)) 11)

(define (up n)
  (let
      ([fun (list-ref listfun 0)])
    (fun n)))

(define (down n)
  (let
      ([fun (list-ref listfun 1)])
    (fun n)))

(test (up 90) 100)
(test (down 110) 100)