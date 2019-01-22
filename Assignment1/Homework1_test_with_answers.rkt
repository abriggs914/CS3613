#lang plait

;; Given the following variant type declaration, define a function
;; called @c{area} to compute the area of a @c{Shape}.

(define pi 3.141592653589793)

(define-type Shape
  [Square  (side : Number)]
  [Circle  (radius : Number)]
  [Triangle (height : Number) (width :  Number)])

(define (area [shape : Shape]) : Number
  (type-case Shape shape
    [(Square s) (* s s)] ;; s^2
    [(Circle r) (* pi (* r r))] ;; pi*r^2
    [(Triangle h w) (* h (* 1/2 w))])) ;; 1/2*b*h


(module+ test
  ;; For coverage
  (test (area (Square 2))  4)
  (test (area (Circle 2))  (* pi 4))
  (test (area (Triangle 2 2))  2)
  ;; Zero height triangles have zero area
  (test (area (Triangle 0 1))  0)
  ;; Negative sides are not allowed (in this implementation)
  (test/exn (area (Square -2))  "Negative")
  (test/exn (area (Circle -2))  "Negative")
  (test/exn (area (Triangle -2 2))  "Negative")
  (test/exn (area (Triangle 2 -2))  "Negative"))


;; -------------------------------------------------------------------
;;
;; Converts a list of ternary digits to a (decimal) number.

(define (pow acc base exp)
  (cond
    [(zero? exp) acc]
    [else (pow (* acc base) base (- exp 1))]))

(define (helper num tot lst)
  (cond
    [(empty? lst) tot]
    [else (helper (+ 1 num) (+ (* (first lst) (pow 1 3 num)) tot) (rest lst))]))


(define (tern->num numbers)
  (helper 0 0 numbers))

(module+ test
  ;; Test coverage
  (test (tern->num  empty)  0)
  (test (tern->num  (list 0))  0)
  (test/exn (tern->num '(3)) "bad digit")

  ;; test other possible single digit lists
  (test (tern->num  (list 1))  1)
  (test (tern->num  (list 2))  2)

  ;; make sure the ordering is correct, least significant digit first
  (test (tern->num  (list 1 0))  1)

  ;; digits after first are not ignored
  (test (tern->num  (list 1 1))  4)

  ;; "Leading" zeros (actually trailing in this format are OK
  (test (tern->num  (list 0 0 0 0))  0)
  (test (tern->num  (list 1 0 0 0))  1)

  ;; Place value works
  (test (tern->num (list 0 0 0 1))  27)

  ;; A few less trivial examples
  (test (tern->num (list 1 1 1 1))  40)
  (test (tern->num (list 1 0 2 1))  46)

  ;; Check errors in positions other than first
  (test/exn (tern->num '(1 2 11 2 0)) "bad digit")

  ;; negative numbers are bad digits
  (test/exn (tern->num '(0 1 -1 2)) "bad digit"))

(define minutes-spent 120)