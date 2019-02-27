#lang plait

#|
flang: NUMBER
| { + flang flang }
| { - flang flang }
| { * flang flang }
| { / flang flang }
| { WITH { ID flang } flang }
| ID
| { FUN { ID } flang }
| { CALL flang flang }
|#

(define-type RFLANG
  [Num  (val : Number)]
  [Add  (l : RFLANG) (r : RFLANG)]
  [Sub  (l : RFLANG) (r : RFLANG)]
  [Mul  (l : RFLANG) (r : RFLANG)]
  [Div  (l : RFLANG) (r : RFLANG)]
  [Id   (name : Symbol)]
  [If (test : RFLANG) (then-part : RFLANG) (else-part : RFLANG)]
  [With (id : Symbol) (named-expr : RFLANG) (bound-body : RFLANG)]
  [Rec (id : Symbol) (named-expr : RFLANG) (bound-body : RFLANG)]
  [Fun  (param : Symbol) (body : RFLANG)]
  [Call (fun : RFLANG) (val : RFLANG)]
  [Set (id : RFLANG)])

(define-type VAL
  [NumV (n : Number)]
  [FunV (arg : Symbol) (body : RFLANG) (env : ENV)]
  [BogusV])

;; (VAL -> Number)
(define (val->number v)
  (type-case VAL v
    [(NumV num) num]
    [else (error 'val-number "not a number")]))

(define-type ENV
  [EmptyEnv]
  [Extend (name : Symbol) (val : (Boxof VAL)) (end : ENV)])

;; (Symbol ENV -> (Boxof VAL))
(define (lookup name env)
  (type-case ENV env
    [(EmptyEnv ) (error 'lookup (string-append "no binding for " (to-string name)))]
    [(Extend id boxed-val rest-env)
            (if (eq? id name) boxed-val (lookup name rest-env))]))

;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
;; ((Number Number -> Number) VAL VAL -> VAL)
(define (arith-op op val1 val2)
  (local
      [(define (NumV->number v)
         (type-case VAL v
           [(NumV n) n]
           [else (error 'arith-op (string-append "expects a number, got: " (to-string v)))]))]
    (NumV (op (NumV->number val1) (NumV->number val2)))))

;; evaluates RFLANG expressions by reducing them to values
;; (RFLANG ENV -> VAL)
(define (eval expr env)
  (type-case RFLANG expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(If test then-part else-part)
        (if (eq? 0 (val->number (eval test env)))
            (eval else-part env)
            (eval then-part env))]
    [(With bound-id named-expr bound-body)
          (eval bound-body
                (Extend bound-id (box (eval named-expr env)) env))]
    [(Rec bound-id named-expr bound-body)
         (eval bound-body
               (extend-rec bound-id named-expr env))]
    [(Id name) (unbox (lookup name env))]
    [(Fun bound-id bound-body)
         (FunV bound-id bound-body env)]
    [(Call fun-expr arg-expr)
          (let ([fval (eval fun-expr env)])
            (type-case VAL fval
              [(FunV bound-id bound-body f-env)
                    (eval bound-body
                          (Extend bound-id (box (eval arg-expr env)) f-env))]
              [else (error 'eval (string-append "`call' expects a function, got: "
                                                (to-string fval)))]))]
    [(Set id) (NumV 0)]));(lookup id env)]))

;;(Symbol RFLANG ENV -> ENV)
(define (extend-rec id expr rest-env)
  (let ([new-cell (box (NumV 42))])
    (let ([new-env (Extend id new-cell rest-env)])
      (let ([value (eval expr new-env)])
        (begin
          (set-box! new-cell value)
          new-env)))))

;; ('a -> 'b)
(define (parse-error sx)
  (error 'parse-sx (string-append "parse error: " (to-string sx))))

;; (S-Exp -> RFLANG)
(define (parse-sx sx)
  (local
      [(define (sx-ref sx n) (list-ref (s-exp->list sx) n))
       (define (px i) (parse-sx (sx-ref sx i)))
       (define (with-rec-pieces)
         (let* ([def (sx-ref sx 1)]
                [id (s-exp->symbol (sx-ref def 0))]
                [val (parse-sx (sx-ref def 1))]
                [expr (px 2)])
           (values id val expr)))]
    (cond
      [(s-exp-number? sx) (Num (s-exp->number sx))]
      [(s-exp-symbol? sx) (Id (s-exp->symbol sx))]
      [(s-exp-match? `(with (SYMBOL ANY) ANY) sx)
       (local [(define-values (id val expr) (with-rec-pieces))]
         (With id val expr))]
      [(s-exp-match? `(rec (SYMBOL ANY) ANY) sx)
       (local [(define-values (id val expr) (with-rec-pieces))]
         (Rec id val expr))]
      [(s-exp-match? `(if ANY ANY ANY) sx)
       (If (px 1) (px 2) (px 3))]
      [(s-exp-match? `(fun (SYMBOL) ANY) sx)
       (let* ([args (sx-ref sx 1)]
              [id (s-exp->symbol (sx-ref args 0))]
              [body (parse-sx (sx-ref sx 2))])
         (Fun id body))]
      [(s-exp-list? sx)
       (let* ([l (lambda () (px 1))]
              [r (lambda () (px 2))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [(call) (Call (l) (r))]
           [else (parse-error sx)]))]
      [else (parse-error sx)])))

;; evaluate a RFLANG program contained in a string
;; (S-Exp -> Number)
(define (run sx)
  (let ([result (eval (parse-sx sx) (EmptyEnv))])
    (type-case VAL result
      [(NumV n) n]
      [else (error 'run
                   (string-append
                    "evaluation returned a non-number " (to-string result)))])))

(define minutes-spent 60)

(module+ test
  (test (run `{with {x 3}
                    {with {f {fun {y} {+ x y}}}
                          {with {x 5}
                                {call f 4}}}})
        7)
  (test (run `{call {with {x 3}
                          {fun {y} {+ x y}}}
                    4})
        7)

  (test (run `{if 0 1 0})  0)
  (test/exn (run `{if {fun {y} y} 1 0})  "not a number")


  (test (run `{rec {fact {fun {n}
                              {if n 
                                  {* n {call fact {- n 1}}}
                                  1}}}
                   {call fact 5}})  120)
  (test/exn (run `{rec {mod {fun {n} {if {< n 2} {* n {call mod {/ n 2}}} 1}}} {call mod 4}}) "parse error: `(< n 2)")
  (test (run `{call {with {x 32}
                          {fun {y} {/ x y}}}
                    4})
        8)
  (test/exn (run `{call {with {x 32}
                          {fun {y} {/ z y}}}
                    4})
        "lookup: no binding for 'z")
  (test/exn (run `{if 1 {fun {y} y} 1}) "run: evaluation returned a non-number (FunV 'y (Id 'y) (EmptyEnv))")
  (test/exn (run `{+ {fun {y} y} {fun {y} y}}) "arith-op: expects a number, got: (FunV 'y (Id 'y) (EmptyEnv))")
  (test/exn (parse-sx (boolean->s-exp #t)) "parse-sx: parse error: `#t")
  (test/exn (run `{call {with {x 7} 8} 7}) "eval: `call' expects a function, got: (NumV 8)"))