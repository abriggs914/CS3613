#lang plait

#|
 CS3613 Homework 4
 Feb.8/19
 Avery Briggs
 3471065
|#

(define-type ArgPair
  [argpair (arg : Symbol) (expr : W*AE)]) ;(Symbol W*AE -> ArgPair)

(define-type W*AE
  [Num  (val : Number)] ; (Number -> W*AE)
  [Add  (l : W*AE) (r : W*AE)] ; (W*AE W*AE -> W*AE)
  [Sub  (l : W*AE) (r : W*AE)] ; (W*AE W*AE -> W*AE)
  [Mul  (l : W*AE) (r : W*AE)] ; (W*AE W*AE -> W*AE)
  [Div  (l : W*AE) (r : W*AE)] ; (W*AE W*AE -> W*AE)
  [Id   (name : Symbol)] ; (Symbol -> W*AE)
  [With (name : Symbol) (val : W*AE) (expr : W*AE)] ;(Symbol W*AE W*AE -> W*AE)
  [With* (argslist : (Listof ArgPair)) (body :  W*AE)]) ; ((Listof ArgPair) W*AE -> W*AE)

; ('a -> 'b)
(define (parse-error sx)
  (error 'parse-sx (string-append "parse error: " (to-string sx))))

; (S-Exp Number -> S-Exp)
(define (sx-ref sx n) (list-ref (s-exp->list sx) n))

; ((Listof S-Exp) (Listof ArgPair) -> (Listof ArgPair))
(define (parse-args arglist acc)
  (if (empty? arglist)
      (reverse acc)
      (let* ([pair (first arglist)]
             [name  (s-exp->symbol (sx-ref pair 0))]
             [expr (parse-sx (sx-ref pair 1))])
        (parse-args (rest arglist) (cons (argpair name expr) acc)))))

; (S-Exp -> W*AE)
(define (parse-sx sx)
  (cond
    [(s-exp-number? sx) (Num (s-exp->number sx))]
    [(s-exp-symbol? sx) (Id (s-exp->symbol sx))]
    [(s-exp-match? `(with (SYMBOL ANY) ANY) sx)
     (let* ([def (sx-ref sx 1)]
            [id (s-exp->symbol (sx-ref def 0))]
            [val (parse-sx (sx-ref def 1))]
            [expr (parse-sx (sx-ref sx 2))])
       (With id val expr))]
    [(s-exp-match? `(with* ((SYMBOL ANY) ...) ANY) sx)
     (let* ([defs (parse-args (s-exp->list (sx-ref sx 1)) empty)]
            [expr (parse-sx (sx-ref sx 2))])
       (With* defs expr))]

    [(s-exp-match? `(ANY ANY ANY) sx)
     (let* ([l (lambda () (parse-sx (sx-ref sx 1)))]
            [r (lambda () (parse-sx (sx-ref sx 2)))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [else (parse-error sx)]))]
    [else (parse-error sx)]))

(module+ test
  (test/exn  (parse-sx `"hi mom") "parse error")
  (test/exn  (parse-sx `{& 1 2}) "parse error")
  (test/exn  (parse-sx `{+ 1 2 3}) "parse error")
  (test/exn (parse-sx `{with* {{x 1} {y 2}}}) "parse error")
  (test (parse-sx `{with* {{x 1} {y 2}} {+ x y}}) (With* (list (argpair 'x (Num 1))
                                                         (argpair 'y (Num 2)))
                                                         (Add (Id 'x) (Id 'y)))))

;; expr[to/from]
; (W*AE Symbol W*AE -> W*AE)
(define (subst expr from to)
  (type-case W*AE expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
           (With bound-id
                 (subst named-expr from to)
                 (if (eq? bound-id from)
                     bound-body
                     (subst bound-body from to)))]
    [(With* arglist bound-body)
     (if (empty? arglist) bound-body 
     (type-case ArgPair (first arglist)
       [(argpair arg expr) ; arg = symbol, expr = W*AE
        (With* (rest arglist)
               (With arg 
                     (subst expr from to)
                            (if (eq? arg from) bound-body
                                (subst bound-body from to))))]))]))
#|
[(With* arglist bound-body)
     (if (empty? arglist) bound-body 
     (type-case ArgPair (first arglist)
       [(argpair arg expr) ; arg = symbol, expr = W*AE
        (With* (map (lambda (arglistpair)
                      (type-case ArgPair (arglistpair)
                        [(argpair arg innerexpr) (argpair arg (subst expr arg bound-body))])) (rest arglist))
               (With arg 
                     (subst expr from to)
                            (if (eq? arg from) bound-body
                                (subst bound-body from to))))]))]
|#

#|(list (parse-args (map (lambda (id) (With* id
                                      (subst id from to)
                                      (if (eq? id from)
                                          bound-body
                                          (subst bound-body from to)))) arglist) bound-body))]))|#




;; evaluate a WAE program contained in an s-expression
; (S-Exp -> Number)
(define (run sx)
  (eval (parse-sx sx)))

;; evaluates WAE expressions
; (W*AE -> Number)
(define (eval expr)
  (type-case W*AE expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
          (eval (subst bound-body
                       bound-id
                       (Num (eval named-expr))))] ; <-***
    [(Id name) (error 'eval (string-append "free identifier: " (to-string name)))]
    [(With* arglist bound-body)
     (eval (if (empty? arglist)
               bound-body
               (type-case ArgPair (first arglist)
                 [(argpair arg expr)
                  (With* (rest arglist)
                         (Num (eval (subst bound-body arg expr))))])))]))
;(trace subst)
;(trace eval)
;; tests
(module+ test
  #|
  (test (run `5) 5)
  (test (run `{+ 5 5}) 10)
  (test (run `{* 5 5}) 25)
  (test (run `{/ 5 5}) 1)
  (test (run `{with {x {+ 5 5}} {+ x x}}) 20)
  (test (run `{with {x 5} {+ x x}}) 10)
  (test (run `{with {x 5} {* x x}}) 25)
  (test (run `{with {x 5} {/ x x}}) 1)
  (test (run `{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) 14)
  (test (run `{with {x 5} {with {y {- x 3}} {+ y y}}}) 4)
  (test (run `{with {x 5} {+ x {with {x 3} 10}}}) 15)
  (test (run `{with {x 5} {+ x {with {x 3} x}}}) 8)
  (test (run `{with {x 5} {+ x {with {y 3} x}}}) 10)
  (test (run `{with {x 5} {with {y x} y}}) 5)
  (test (run `{with {x 5} {with {x x} x}}) 5)
  (test/exn (run `{with {x 1} y}) "free identifier")|#
  (test (run `{with* {{x 1} {y 2}} {+ x y}}) 3)
  (test (run `{with* {{x 5}
                      {y {- x 3}}} {+ y y}})  4)
  (test (run `{with* {{x 5}
                      {y x}} y}) 5))

(define minutes-spent 180)

#|
;;
(define a (With* (list (argpair 'x (Num 1)) (argpair 'y (Num 2))) (Add (Id 'x) (Id 'y)))) ; => W*AE
(define b (map (λ (item) (symbol->s-exp item)) (list 'x 'y))) ; => listof s-exp
(define c (list 'x 'y)) ; => listof symbol
(define d (list (argpair 'y (Num 2)) (argpair 'x (Num 1)))) ; =>listof argpair
;(define e (parse-args b d)) ; => not working listof argpair
(define f (With* d a)) ; => W*AE
(define g (subst f (first c) f)) ; => W*AE
(define h (eval g)) ; => 3
(define i (run `{with* {{x 5} {y {- x 3}}} {+ y y}})) ; => 4
;;
|#