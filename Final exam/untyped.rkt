#lang plait #:untyped
(define (make-test self)
  (let ([test (lambda (x) (if x x (self self)))])
    (lambda (arg)
      (if arg
      arg
      (test (not arg))))))

(display "after define make-test\n")

(define test (make-test make-test))
(display "after define test\n")

(display (test #f))

;after define make-test
;after define test
;#t

(let ([fac
       (lambda (m)
         (let ([facY
                (lambda (facX n)
                  (if (zero? n)
                      1
                      (* n (facX facX (- n 1)))))])
           (facY facY m)))])
  (begin
    (fac 7)))