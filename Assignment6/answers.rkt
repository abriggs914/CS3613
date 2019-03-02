#lang plait

#|
rflang: NUMBER
        | { + rflang rflang }
        | { - rflang rflang }
        | { * rflang rflang }
        | { / rflang rflang }
        | { WITH { ID rflang } rflang+ }
        | { REC { ID rflang } rflang+ }
        | { SET! ID RFLANG }
        | ID
        | { IF RFLANG RFLANG RFLANG }
        | { FUN { ID } rflang }
        | { CALL flang rflang }
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
  [RFun  (param : Symbol) (body : RFLANG)]
  [Set  (id : Symbol) (val : RFLANG)]
  [Begin (body : (Listof RFLANG))]
  [Call (fun : RFLANG) (val : RFLANG)])

(define-type VAL
  [BogusV]
  [NumV (n : Number)]
  [FunV (arg : Symbol) (body : RFLANG) (env : ENV) (byref : Boolean)])

(define-type ENV
  [EmptyEnv]
  [Extend (name : Symbol) (val : (Boxof VAL)) (end : ENV)])

(define (val->number v)
  (type-case VAL v
    [(NumV num) num]
    [else (error 'val-number "not a number")]))

(define (arith-op op val1 val2)
  (NumV (op (val->number val1) (val->number val2))))

(define (lookup name env)
  (type-case ENV env
    [(EmptyEnv) (error 'lookup (string-append "no binding for " (to-string name)))]
    [(Extend id boxed-val rest-env)
            (if (eq? id name) boxed-val (lookup name rest-env))]))

(module+ test
  (test/exn (lookup 'x (EmptyEnv)) "no binding"))

(define (eval-body [expr-list : (Listof RFLANG)] [env : ENV])
  (let ([last-val (box [BogusV])])
    (begin
      (map (lambda ([exp : RFLANG])
             (set-box! last-val (eval exp env))) expr-list)
      (unbox last-val))))

(define (get-box arg env)
  (type-case RFLANG arg
    [(Id sym) (lookup sym env)]
    [else (error 'rfun "non-identifier")]))

;; evaluates RFLANG expressions by reducing them to values
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
    [(Set id expr)
         (let ([the-box (lookup id env)]
               [the-val (eval expr env)])
           (begin
             (set-box! the-box the-val)
             [BogusV]))]
    [(Begin expr-list)
           (eval-body expr-list env)]
    [(Id name) (unbox (lookup name env))]
    [(RFun bound-id bound-body)
          (FunV bound-id bound-body env #t)]
    [(Fun bound-id bound-body)
         (FunV bound-id bound-body env #f)]
    [(Call fun-expr arg-expr)
          (let ([fval (eval fun-expr env)])
            (type-case VAL fval
              [(FunV bound-id bound-body f-env by-ref?)
                    (let ([new-cell (if by-ref? (get-box arg-expr env)
                                        (box (eval arg-expr env)))])
                      (eval bound-body (Extend bound-id new-cell f-env)))]
              [else (error 'eval (string-append "`call' expects a function, got: "
                                                (to-string fval)))]))]))
(define (extend-rec id expr rest-env)
  (let ([new-cell (box (NumV 42))])
    (let ([new-env (Extend id new-cell rest-env)])
      (let ([value (eval expr new-env)])
        (begin
          (set-box! new-cell value)
          new-env)))))

(define (parse-error sx)
  (error 'parse-sx (string-append "parse error: " (to-string sx))))

(module+ test
  (test/exn (parse-sx `{blah blah blah}) "parse error"))

(define (parse-sx sx)
  (local
      [(define (sx-ref sx n) (list-ref (s-exp->list sx) n))
       (define (px i) (parse-sx (sx-ref sx i)))
       (define (with-rec-pieces)
         (let* ([def (sx-ref sx 1)]
                [id (s-exp->symbol (sx-ref def 0))]
                [val (parse-sx (sx-ref def 1))]
                [expr (px 2)])
           (values id val expr)))
       (define (fun-rfun-pieces)
         (let* ([args (sx-ref sx 1)]
                [id (s-exp->symbol (sx-ref args 0))]
                [body (parse-sx (sx-ref sx 2))])
           (values id body)))]
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
       (local [(define-values (id body) (fun-rfun-pieces))]
         (Fun id body))]
      [(s-exp-match? `(rfun (SYMBOL) ANY) sx)
       (local [(define-values (id body) (fun-rfun-pieces))]
         (RFun id body))]
      [(s-exp-match? `(set! SYMBOL ANY) sx)
       (Set (s-exp->symbol (sx-ref sx 1)) (px 2))]
      [(s-exp-match? `(begin ANY ...) sx)
       (let ([body (map parse-sx (rest (s-exp->list sx)))])
         (Begin body))]
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

(module+ test
  (test/exn (parse-sx `"hello world") "parse error"))

;; evaluate a RFLANG program contained in a string
(define (run sx)
  (let ([result (eval (parse-sx sx) (EmptyEnv))])
    (type-case VAL result
      [(NumV n) n]
      [else (error 'run
                   (string-append
                    "evaluation returned a non-number " (to-string result)))])))
(module+ test
  (test/exn (run `{fun {x} x}) "non-number")
  (test (run `{with {x 3}
                    {with {f {fun {y} {+ x y}}}
                          {with {x 5}
                                {call f 4}}}})
        7)

  (test (run `{call {with {x 3}
                          {fun {y} {+ x y}}}
                    4})
        7)

  (test (run `{if 0 1 {/ 1 1}})  1)
  (test/exn (run `{if {fun {y} y} 1 0})  "not a number")
  (test (run `{rec {fact {fun {n}
                              {if n
                                  {* n {call fact {- n 1}}}
                                  1}}}
                   {call fact 5}})  120)

  (test (run `{with {x 1}
                    {with {y {set! x 2}}
                          x}}) 2)

  (test (run `{with {x 1}
                    {begin
                      {set! x 2}
                      x}}) 2)

  (test (run `{with {make-counter
                     {fun {initial}
                          {with {c initial}
                                {fun {dummy}
                                     {begin
                                       {set! c {+ 1 c}}
                                       c}}}}}
                    {with {c1 {call make-counter 0}}
                          {with  {c2 {call make-counter 0}}
                                 {*  {* {call c1 0} {call c1 0}}
                                     {* {call c2 0} {call c1 0}}}}}})
        6)

  (test (run `{with {x 1}
                    {with {f {fun {y} {set! y 2}}}
                          {begin {call f x} x}}}) 1)
  (test (run `{with {x 1}
                    {with {f {rfun {y} {set! y 2}}}
                          {begin {call f x} x}}})
        2)

  (test/exn (run `{with {x 1}
                        {with {f {rfun {y} {set! y 2}}}
                              {begin {call f 1} x}}})  "non-identifier")

  (test/exn (run `{call {rfun {x} x} {/ 4 0}})  "non-identifier")

  (test/exn (run `{call 5 {/ 6 0}}) "`call' expects a function")

  (test (run `{with {swap! {rfun {x}
                                 {rfun {y}
                                       {with {tmp x}
                                             {begin
                                               {set! x y}
                                               {set! y tmp}}}}}}
                    {with  {a 1}
                           {with  {b 2}
                                  {begin
                                    {call {call swap! a} b}
                                    {+ a {* 10 b}}}}}})
        12))

(define minutes-spent 120)