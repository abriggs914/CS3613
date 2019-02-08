#lang plait

(let ((x 3))
  (let ((f (λ (y) (+ x y))))
    (let ((x 5)) ; inner x is not bound
      (f 4)))) ; evaluates to 7

(define (blah func val)
 (func val))

(let ((x 3))
  (let ((f (λ (y) (+ x y))))
    (let ((x 5)) ; inner x is not bound
      (blah f 4)))) ; evaluates to 7