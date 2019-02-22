#lang plait

  (define-type WAE
    [Num  (val : Number)]
    [Add  (l : WAE) (r : WAE)]
    [Sub  (l : WAE) (r : WAE)]
    [Mul  (l : WAE) (r : WAE)]
    [Div  (l : WAE) (r : WAE)]
    [Id   (name : Symbol)]
    [With (name : Symbol) (val : WAE) (expr : WAE)])
(define (parse-error sx)
  (error 'parse-sx (string-append "parse error: " (to-string sx))))

(define (sx-ref sx n) (list-ref (s-exp->list sx) n))
  
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
    [(s-exp-match? `(ANY ANY ANY) sx)
     (let* ([l (λ () (parse-sx (sx-ref sx 1)))]
            [r (λ () (parse-sx (sx-ref sx 2)))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [else (parse-error sx)]))]
    [else (parse-error sx)]))

  ;; expr[to/from]
  (define (subst expr from to)
    (type-case WAE expr
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
               (subst bound-body from to)))]))
  ;; evaluates WAE expressions 
  (define (eval expr)
    (type-case WAE expr
      [(Num n) n]
      [(Add l r) (+ (eval l) (eval r))]
      [(Sub l r) (- (eval l) (eval r))]
      [(Mul l r) (* (eval l) (eval r))]
      [(Div l r) (/ (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))] ; <-***
      [(Id name) (error 'eval (string-append "free identifier: " (to-string name)))]))

  ;; evaluate a WAE program contained in an s-expression
  (define (run sx)
    (eval (parse-sx sx)))

(test (run `5) 5)
(test (run `{+ 5 5}) 10)
(test (run `{with {x {+ 5 5}} {+ x x}}) 20)
(test (run `{with {x 5} {+ x x}}) 10)
(test (run `{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) 14)
(test (run `{with {x 5} {with {y {- x 3}} {+ y y}}}) 4)
(test (run `{with {x 5} {+ x {with {x 3} 10}}}) 15)
(test (run `{with {x 5} {+ x {with {x 3} x}}}) 8)
(test (run `{with {x 5} {+ x {with {y 3} x}}}) 10)
(test (run `{with {x 5} {with {y x} y}}) 5)
(test (run `{with {x 5} {with {x x} x}}) 5)
(test/exn (run `{with {x 1} y}) "free identifier")

(define (find-ids wae)
  (type-case WAE wae
    [(Add l r) (append (find-ids l) (find-ids r))]
    [(With name val expr) (append (list name) (find-ids expr))]
    [else empty]))

; procedure to compare two numbers.
; call ((pair a b) X) where x is (Number Number -> Y)
(define (pair a b)
  (lambda (x) (x a b)))

(test ((pair 1 3) +) 4)
(test ((pair 1 3) -) -2)
(test ((pair 2 3) *) 6)

(find-ids (parse-sx `{with {x 5} {with {y x} y}}))
(find-ids (parse-sx `5))