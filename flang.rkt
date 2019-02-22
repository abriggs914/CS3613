#lang plait
  (define-type FLANG
    [Num  (val : Number)]
    [Add  (l : FLANG) (r : FLANG)]
    [Sub  (l : FLANG) (r : FLANG)]
    [Mul  (l : FLANG) (r : FLANG)]
    [Div  (l : FLANG) (r : FLANG)]
    [Id   (name : Symbol)]
    [With (id : Symbol) (named-expr : FLANG) (bound-body : FLANG)]
    [Fun  (param : Symbol) (body : FLANG)]
    [Call (fun : FLANG) (val : FLANG)]) ; first type!
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
    [(s-exp-match? `(fun (SYMBOL) ANY) sx)
     (let* ([args (sx-ref sx 1)]
            [id (s-exp->symbol (sx-ref args 0))]
            [body (parse-sx (sx-ref sx 2))])
       (Fun id body))]
    [(s-exp-list? sx)
     (let* ([l (lambda () (parse-sx (sx-ref sx 1)))]
            [r (lambda () (parse-sx (sx-ref sx 2)))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [(call) (Call (l) (r))]
           [else (parse-error sx)]))]
    [else (parse-error sx)]))

  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (type-case FLANG expr
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
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]))
  ;; gets a Racket numeric binary operator, and uses it within a FLANG
  ;; `Num' wrapper (note H.O type)
  (define (arith-op op expr1 expr2)
    (local
        [(define (Num->number e)
           (type-case FLANG e
             [(Num n) n]
             [else (error 'arith-op "expects a number")]))]
      (Num (op (Num->number expr1) 
               (Num->number expr2)))))
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (type-case FLANG expr
      [(Num n) expr]                             ; <- change here
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]         ; <- no `(Num ...)'
      [(Id name) (error 'eval "free identifier")]
      [(Fun bound-id bound-body) expr]           ; <- similar to `Num'
      [(Call fun arg-expr) 
            (type-case FLANG fun
              [(Fun bound-id bound-body)
                   (eval (subst bound-body                   ; <- just like `with'
                                bound-id
                                (eval arg-expr)))]
              [else (error 'eval "`call' expects a function")])]))
  (define (run sx)
    (let ([result (eval (parse-sx sx))])
      (type-case FLANG result
        [(Num n) n]
        [else (error 'run "evaluation returned a non-number")])))
(run `{with {f {fun {y} {+ x y}}}
        {with {x 7}
          {call f 1}}})
(trace eval)