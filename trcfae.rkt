#lang plait

(define-type FAE
  [Num (n : Number)]
  [Bool (b : Boolean)]
  [Not (expr : FAE)]
  [Add (lhs : FAE)
       (rhs : FAE)]
  [Sub (lhs : FAE)
       (rhs : FAE)]
  [Mul (lhs : FAE)
       (rhs : FAE)]
  [If0 (test-expr : FAE)
       (then-expr : FAE)
       (else-expr : FAE)]
  [Rec (name : Symbol)
       (ty : TE)
       (rhs-expr : FAE)
       (body-expr : FAE)]
  [Id (name : Symbol)]
  [Fun (param : Symbol)
       (argty : TE)
       (body : FAE)]
  [Call (fun-expr : FAE)
       (arg-expr : FAE)])

(define-type TE
  [NumTE]
  [BoolTE]
  [ArrowTE (arg : TE)
           (result : TE)])

(define-type FAE-Value
  [NumV (n : Number)]
  [BoolV (b : Boolean)]
  [ClosureV (param : Symbol)
            (body : FAE)
            (env : Env)])

(define-type Env
  [EmptyEnv]
  [Bind (name : Symbol)
        (value : FAE-Value)
        (rest : Env)]
  [RecBind (name : Symbol)
           (value-box : (Boxof FAE-Value))
           (rest : Env)])

(define-type Type
  [NumT]
  [BoolT]
  [ArrowT (arg : Type)
          (result : Type)])

(define-type TypeEnv
  [EmptyTypeEnv]
  [BindType (name : Symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

(define (numzero? x) (= 0 (NumV-n x)))

;; eval : FAE Env -> FAE-Value
(define (eval a-fae env)
  (type-case FAE a-fae
    [(Num n) (NumV n)]
    [(Bool b) (BoolV b)]
    [(Not e) (BoolV (not (BoolV-b (eval e env))))]
    [(Add l r) (num+ (eval l env) (eval r env))]
    [(Sub l r) (num- (eval l env) (eval r env))]
    [(Mul l r) (num* (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(If0 test then-part else-part)
     (if (numzero? (eval test env))
         (eval then-part env)
         (eval else-part env))]
    [(Rec bound-id type named-expr body-expr)
     (let* ([value-holder (box (NumV 42))]
            [new-env (RecBind bound-id value-holder env)])
       (begin
         (set-box! value-holder (eval named-expr new-env))
         (eval body-expr new-env)))]
    [(Fun param arg-te body-expr)
         (ClosureV param body-expr env)]
    [(Call fun-expr arg-expr)
         (local [(define fun-val
                   (eval fun-expr env))
                 (define arg-val
                   (eval arg-expr env))]
           (eval (ClosureV-body fun-val)
                   (Bind (ClosureV-param fun-val)
                         arg-val
                         (ClosureV-env fun-val))))]))

;; num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

(define (num+ x y) (num-op + '+ x y))
(define (num- x y) (num-op - '- x y))
(define (num* x y) (num-op * '* x y))

(define (lookup name env)
  (type-case Env env
    [(EmptyEnv) (error 'lookup "free variable")]
    [(Bind sub-name num rest-env)
          (if (equal? sub-name name)
              num
              (lookup name rest-env))]
    [(RecBind sub-name val-box rest-env)
             (if (equal? sub-name name)
                 (unbox val-box)
                 (lookup name rest-env))]))

;; ----------------------------------------

(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [(EmptyTypeEnv) (error 'type-lookup "free variable, so no type")]
    [(BindType name ty rest)
           (if (equal? name-to-find name)
               ty
               (type-lookup name-to-find rest))]))

;; ----------------------------------------

(define (parse-type te)
  (type-case TE te
    [(NumTE) (NumT)]
    [(BoolTE) (BoolT)]
    [(ArrowTE a b) (ArrowT (parse-type a)
                           (parse-type b))]))

(define (type-error fae msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string fae)
                      (string-append " not "
                                     msg)))))

(define (type-assert exprs type env result) : Type
  (cond
    [(empty? exprs) result]
    [(not (equal? (typecheck (first exprs) env) type))
     (type-error (first exprs) (type-to-string type))]
    [else (type-assert (rest exprs) type env result)]))

(define (type-to-string [type : Type])
  (type-case Type type
    [(BoolT) "bool"]
    [(NumT) "num"]
    [else (to-string type)]))

(define (typecheck [fae : FAE] [env : TypeEnv]) : Type
  (type-case FAE fae
    [(Num n) (NumT)]
    [(Bool b) (BoolT)]
    [(Not e) (type-assert (list e) (BoolT) env (BoolT))]
    [(Add l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Sub l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Mul l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Id name) (type-lookup name env)]
    [(If0 test-expr then-expr else-expr)
     (let* ([test-type (type-assert (list test-expr) (NumT) env (NumT))]
            [then-type (typecheck then-expr env)]
            [else-type (type-assert (list else-expr) then-type env then-type)])
       else-type)]
    [(Rec name ty rhs-expr body-expr)
     (let* ([rhs-ty (parse-type ty)]
            [new-env (BindType name rhs-ty env)])
       (type-assert (list rhs-expr) rhs-ty new-env (typecheck body-expr new-env)))]
    [(Fun name te body)
     (let* ([arg-type (parse-type te)]
            [body-type (typecheck body (BindType name arg-type env))])
       (ArrowT arg-type body-type))]
    [(Call fn arg)
     (type-case Type (typecheck fn env)
       [(ArrowT arg-type result-type)
        (type-assert (list arg) arg-type env result-type)]
       [else (type-error fn "function")])]))

;; ----------------------------------------
;; parse : S-expr -> FAE
(define (parse-error sx)
  (error 'parse (string-append "parse error: " (to-string sx))))

(define (sx-ref sx n) (list-ref (s-exp->list sx) n))

(define (parse sx)
  (local
      [(define (px i) (parse (sx-ref sx i)))]
    (cond
      [(s-exp-number? sx) (Num (s-exp->number sx))]
      [(s-exp-symbol? sx)
       (let ([sym (s-exp->symbol sx)])
         (case sym
           [(true) (Bool #t)]
           [(false) (Bool #f)]
           [else (Id sym)]))]
      [(s-exp-match? `(fun (SYMBOL : ANY) ANY) sx)
       (let* ([args (sx-ref sx 1)]
              [id (s-exp->symbol (sx-ref args 0))]
              [te (parse-te (sx-ref args 2))]
              [body (px 2)])
         (Fun id te body))]
      [(s-exp-match? `(rec (SYMBOL : ANY) ANY ANY) sx)
       (let* ([args (sx-ref sx 1)]
              [id (s-exp->symbol (sx-ref args 0))]
              [te (parse-te (sx-ref args 2))]
              [rhs (px 2)]
              [body (px 3)])
         (Rec id te rhs body))]
      [(s-exp-list? sx)
       (case (s-exp->symbol (sx-ref sx 0))
         [(+) (Add (px 1) (px 2))]
         [(-) (Sub (px 1) (px 2))]
         [(call) (Call (px 1) (px 2))]
         [(if0) (If0 (px 1) (px 2) (px 3))]
         [(not) (Not (px 1))]
         [else (parse-error sx)])]
      [else (parse-error sx)])))

(define (parse-te sx)
  (cond
    [(s-exp-symbol? sx)
     (case (s-exp->symbol sx)
       [(num) (NumTE)]
       [(bool) (BoolTE)])]
    [(s-exp-match? `(ANY -> ANY) sx)
     (ArrowTE (parse-te (sx-ref sx 0)) (parse-te (sx-ref sx 2)))]))

(module+ test
  (print-only-errors #t)
  (test (parse `3) (Num 3))
  (test (parse `x) (Id 'x))
  (test (parse `{+ 1 2}) (Add (Num 1) (Num 2)))
  (test (parse `{- 1 2}) (Sub (Num 1) (Num 2)))
  (test (parse `{fun {x : num} x}) (Fun 'x (NumTE) (Id 'x)))
  (test (parse `{call f 2}) (Call (Id 'f) (Num 2)))
  (test (parse `{if0 0 1 2}) (If0 (Num 0) (Num 1) (Num 2)))

  (test/exn (parse `"foo") "parse error")
  (test/exn (parse `{foo}) "parse error")
  (test (parse
         `{call {fun {x : num}
                     {call {fun {f : {num -> num}}
                                {+ {call f 1}
                                   {call {fun {x : num}
                                              {call f 2}}
                                         3}}}
                           {fun {y : num} {+ x y}}}}
                0})
        (Call (Fun 'x (NumTE)
                   (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                      (Add (Call (Id 'f) (Num 1))
                                           (Call (Fun 'x (NumTE)
                                                      (Call (Id 'f)
                                                            (Num 2)))
                                                 (Num 3))))
                                 (Fun 'y (NumTE)
                                      (Add (Id 'x) (Id 'y)))))
              (Num 0)))

  (define fib-rec
    (Rec 'fib (ArrowTE (NumTE) (NumTE))
         (Fun 'x (NumTE)
              (If0 (Id' x)
                   (Num 1)
                   (If0 (Sub (Id 'x) (Num 1))
                        (Num 1)
                        (Add (Call (Id 'fib) (Sub (Id 'x) (Num 1)))
                             (Call (Id 'fib) (Sub (Id 'x) (Num 2)))))))
         (Call (Id 'fib) (Num 4))))
  

    (define fib-rec-concrete
      `{rec {fib : {num -> num}}
            {fun {x : num}
                 {if0 x 1
                      {if0 {- x 1}
                           1
                           {+ {call fib {- x 1}}
                              {call fib {- x 2}}}}}}
            {call fib 4}})
    (test (parse fib-rec-concrete) fib-rec))



;; ----------------------------------------

(module+ test
  (print-only-errors #t)
  (test/exn (eval (Id 'x) (EmptyEnv)) "free variable")

  (test/exn (typecheck (Id 'x) (EmptyTypeEnv))  "free variable")

  (test (eval (Not (Bool #f)) (EmptyEnv))
        (BoolV #t))

  (test/exn (typecheck (Not (Num 1)) (EmptyTypeEnv)) "not bool")
  
  (test (typecheck (Not (Bool #f)) (EmptyTypeEnv))
        (BoolT))
  
  (test (eval (Num 10)
                (EmptyEnv))
        (NumV 10))
  (test (eval (Add (Num 10) (Num 17))
                (EmptyEnv))
        (NumV 27))
  (test (eval (Sub (Num 10) (Num 7))
                (EmptyEnv))
        (NumV 3))
  (test (eval (Call (Fun 'x (NumTE) (Add (Id 'x) (Num 12)))
                      (Add (Num 1) (Num 17)))
                (EmptyEnv))
        (NumV 30))
  (test (eval (Id 'x)
                (Bind 'x (NumV 10) (EmptyEnv)))
        (NumV 10))

  (test (eval (Call (Fun 'x (NumTE)
                           (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                      (Add (Call (Id 'f) (Num 1))
                                           (Call (Fun 'x (NumTE)
                                                      (Call (Id 'f)
                                                            (Num 2)))
                                                 (Num 3))))
                                 (Fun 'y (NumTE)
                                      (Add (Id 'x) (Id 'y)))))
                      (Num 0))
                (EmptyEnv))
        (NumV 3))

  (test/exn (eval (Id 'x) (EmptyEnv))
            "free variable")

  (test (typecheck (Num 10) (EmptyTypeEnv))
        (NumT))

  (test (typecheck (Add (Num 10) (Num 17)) (EmptyTypeEnv))
        (NumT))
  (test (typecheck (Sub (Num 10) (Num 7)) (EmptyTypeEnv))
        (NumT))

  (test/exn (typecheck (Add (Bool #f) (Num 17)) (EmptyTypeEnv)) "not num")
  (test/exn (typecheck (Sub (Bool #f) (Num 17)) (EmptyTypeEnv)) "not num")
  (test/exn (typecheck (Add (Num 17) (Bool #f)) (EmptyTypeEnv)) "not num")
  (test/exn (typecheck (Sub (Num 17) (Bool #f)) (EmptyTypeEnv)) "not num")

  (test (typecheck (Fun 'x (NumTE) (Add (Id 'x) (Num 12))) (EmptyTypeEnv))
        (ArrowT (NumT) (NumT)))

  (test/exn (typecheck (Call (Fun 'x (NumTE) (Id 'x)) (Bool #t)) (EmptyTypeEnv)) "no type")

  (test (typecheck (Fun 'x (NumTE) (Fun 'y (BoolTE) (Id 'x))) (EmptyTypeEnv))
        (ArrowT (NumT) (ArrowT (BoolT)  (NumT))))

  (test (typecheck (Call (Fun 'x (NumTE) (Add (Id 'x) (Num 12)))
                         (Add (Num 1) (Num 17)))
                   (EmptyTypeEnv))
        (NumT))

  (test (typecheck (Fun 'x (NumTE) (Fun 'y (BoolTE) (Id 'x))) (EmptyTypeEnv))
        (ArrowT (NumT) (ArrowT (BoolT)  (NumT))))

  (test (typecheck (Call (Fun 'x (NumTE)
                              (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                         (Add (Call (Id 'f) (Num 1))
                                              (Call (Fun 'x (NumTE) (Call (Id 'f) (Num 2)))
                                                    (Num 3))))
                                    (Fun 'y (NumTE)
                                         (Add (Id 'x)
                                              (Id' y)))))
                         (Num 0))
                   (EmptyTypeEnv))
        (NumT))

  (test/exn (typecheck (Call (Num 1) (Num 2)) (EmptyTypeEnv))
            "no type")

  (test/exn (typecheck (Add (Fun 'x (NumTE) (Num 12))
                            (Num 2))
                       (EmptyTypeEnv))
            "no type")

  ;; Added coverage test for type-to-string
  (test/exn (typecheck (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                  (Call (Id 'f) (Num 1))) (Num 1)) (EmptyTypeEnv)) "not (ArrowT (NumT) (NumT))")

  ;; Tests for if0

  (test (eval (If0 (Num 0) (Num 1) (Num 0)) (EmptyEnv)) (NumV 1))
  (test (eval (If0 (Num 1) (Num 1) (Num 0)) (EmptyEnv)) (NumV 0))
  (test (typecheck (If0 (Num 0) (Num 1) (Num 0)) (EmptyTypeEnv)) (NumT))
  (test/exn (typecheck (If0 (Num 0) (Fun 'x (NumTE) (Id 'x)) (Num 0)) (EmptyTypeEnv)) "ArrowT")
  (test/exn (typecheck (If0 (Fun 'x (NumTE) (Id 'x)) (Num 0) (Num 0)) (EmptyTypeEnv)) "not num")

  ;; Tests for Rec
  (define fact-rec
    (Rec 'fact (ArrowTE (NumTE) (NumTE))
         (Fun 'n (NumTE)
              (If0 (Id 'n)
                   (Num 1)
                   (Mul (Id 'n) (Call (Id 'fact) (Sub (Id 'n) (Num 1))))))
         (Call (Id 'fact) (Num 5))))

  (test (typecheck fib-rec (EmptyTypeEnv)) (NumT))
  (test (eval fib-rec (EmptyEnv)) (NumV 5))
  (test (typecheck fact-rec (EmptyTypeEnv)) (NumT))
  (test (eval fact-rec (EmptyEnv)) (NumV 120))

  (test/exn (typecheck (Rec 'x (NumTE)
                            (Fun 'y (NumTE) (Num 3))
                            (Num 10))
                       (EmptyTypeEnv))
          "no type")

  ;; Contrived test to get full coverage of lookup
  (test (eval (Rec 'x (NumTE)
                   (Num 10)
                   (Rec 'y (NumTE)
                        (Num 10)
                        (Id 'x)))
              (EmptyEnv))
            (NumV 10)))

(module+ test
  (define (run s-expr)
    (eval (parse s-expr) (EmptyEnv)))

  (define (check s-expr)
    (typecheck (parse s-expr) (EmptyTypeEnv)))

  (test/exn (run `x) "free variable")

  (test/exn (check `x) "free variable")

  (test (run `{not false})  (BoolV #t))

  (test/exn (check `{not 1}) "not bool")

  (test (check `{not false}) (BoolT))

  (test (run `10)  (NumV 10))
  (test (run `{+ 10 17}) (NumV 27))

  (test (run `{call {fun {x : num} {+ x 12}} {+ 1 17}}) (NumV 30))

  (define concrete1 `{call {fun {x : num}
                                {call {fun {f : {num -> num}}
                                           {+ {call f 1}
                                              {call {fun {x : num}
                                                         {call f 2}}
                                                    3}}}
                                      {fun {y : num} {+ x y}}}}
                           0})
    (test (run concrete1) (NumV 3))

  (test (check `10)  (NumT))

  (test (check `{+ 10 17})  (NumT))

  (test/exn (check `{+ false 17}) "not num")
  (test/exn (check `{+ 17 false}) "not num")

  (test (check `{fun {x : num} {+ x 12}}) (ArrowT (NumT) (NumT)))

  (test/exn (check `{call {fun {x : num} x} true}) "no type")

  (test (check `{fun {x : num} {fun {y : bool} x}})  (ArrowT (NumT) (ArrowT (BoolT)  (NumT))))

  (test (check `{call {fun {x : num} {+ x 12}} {+ 1 17}}) (NumT))

  (test (check `{fun {x : num} {fun {y : bool} x}}) (ArrowT (NumT) (ArrowT (BoolT)  (NumT))))

  (test (check concrete1) (NumT))

  (test/exn (check `{call 1 2}) "no type")

  (test/exn (check `{+ {fun {x : num} 12} 2}) "no type")

  (test (run fib-rec-concrete) (NumV 5))
  (test (check fib-rec-concrete) (NumT)))