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
     (let* ([l (λ () (parse-sx (sx-ref sx 1)))]
            [r (λ () (parse-sx (sx-ref sx 2)))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [(call) (Call (l) (r))]
           [else (parse-error sx)]))]
    [else (parse-error sx)]))

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
  ;; a type for substitution caches:
  (define-type Binding
  	       [bind (name : Symbol) (val : FLANG)])

  (define-type-alias SubstCache (Listof Binding))
  (define empty-subst empty)
;
  (define (extend id expr sc)
    (cons (bind id expr) sc))
;
(define (lookup name sc)
  (if (empty? sc)
      (error 'lookup (string-append "no binding for " (symbol->string name)))
      (type-case Binding (first sc)
        [(bind first-name first-val)
              (if (eq? name first-name)
                  first-val
                  (lookup name (rest sc)))])))
  ;; evaluates FLANG expressions by reducing them to expressions
  (define (eval expr sc)
    (type-case FLANG expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l sc) 
			   (eval r sc))]
      [(Sub l r) (arith-op - (eval l sc) (eval r sc))]
      [(Mul l r) (arith-op * (eval l sc) (eval r sc))]
      [(Div l r) (arith-op / (eval l sc) (eval r sc))]
      [(With bound-id named-expr bound-body)
       (eval bound-body
             (extend bound-id (eval named-expr sc) sc))]
      [(Id name) (lookup name sc)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let ([fval (eval fun-expr sc)])
         (type-case FLANG fval
           [(Fun bound-id bound-body)
            (eval bound-body
                  (extend bound-id (eval arg-expr sc) sc))]
           [else (error 'eval
                        (string-append "`call' expects a function, got: "
                                (to-string fval)))]))]))
  ;; evaluate a FLANG program contained in a s-expr
  (define (run s-exp)
    (let ([result (eval (parse-sx s-exp) empty-subst)])
      (type-case FLANG result
        [(Num n) n]
        [else (error 'run
                     (string-append "evaluation returned a non-number: "
                                    (to-string result)))])))
(test (run `{call {fun {x} {+ x 1}} 4}) 5)

(test (run `{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}})   
      4)

(test (run `{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}})
      7)