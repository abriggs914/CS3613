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
  [Pair (left : FAE)  (right : FAE)]
  [Fst (v : FAE)]
  [Snd (v : FAE)]
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
        (arg-expr : FAE)]
  [WithType (name : Symbol)
             (var1-name : Symbol)
             (var1-ty : TE)
             (var2-name : Symbol)
             (var2-ty : TE)
             (body-expr : FAE)]
  [TypeCase (name : Symbol)
            (dispatch-expr : FAE)
            (var1-name : Symbol)
            (bind1-name : Symbol)
            (rhs1-expr : FAE)
            (var2-name : Symbol)
            (bind2-name : Symbol)
            (rhs2-expr : FAE)])

(define-type TE
  [NumTE]
  [BoolTE]
  [PairTE (left : TE)
          (right : TE)]
  [ArrowTE (arg : TE)
           (result : TE)]
  [IdTE (name : Symbol)])

(define-type FAE-Value
  [NumV (n : Number)]
  [BoolV (b : Boolean)]
  [PairV (l : FAE-Value) (r : FAE-Value)]
  [ClosureV (param : Symbol)
            (body : FAE)
            (env : Env)]
  [VariantV (right? : Boolean)
            (val : FAE-Value)]
  [ConstructorV (right? : Boolean)])

(define-type Env
  [mtSub]
  [aSub (name : Symbol)
        (value : FAE-Value)
        (rest : Env)]
  [aRecSub (name : Symbol)
           (value-box : (Boxof FAE-Value))
           (rest : Env)])

(define-type Type
  [NumT]
  [BoolT]
  [PairT (left : Type) (right : Type)]
  [ArrowT (arg : Type)
          (result : Type)]
  [IdT (name : Symbol)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : Symbol)
         (type : Type)
         (rest : TypeEnv)]
  [tBind (name : Symbol)
         (var1-name : Symbol)
         (var1-type : Type)
         (var2-name : Symbol)
         (var2-type : Type)
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
    [(Pair l r) (PairV (eval l env) (eval r env))]
    [(Fst ex)
     (type-case FAE-Value (eval ex env)
       [(PairV l r) l]
       [else (type-error 'eval "not a pair")])]
    [(Snd ex)
     (type-case FAE-Value (eval ex env)
       [(PairV l r) r]
       [else (type-error 'eval "not a pair")])]
    [(If0 test then-part else-part)
     (if (numzero? (eval test env))
         (eval then-part env)
         (eval else-part env))]
    [(Rec bound-id type named-expr body-expr)
     (let* ([value-holder (box (NumV 42))]
            [new-env (aRecSub bound-id value-holder env)])
       (begin
         (set-box! value-holder (eval named-expr new-env))
         (eval body-expr new-env)))]
    [(Fun param arg-te body-expr)
         (ClosureV param body-expr env)]
    [(Call fun-expr arg-expr)
     (let* ([fun-val (eval fun-expr env)]
            [arg-val (eval arg-expr env)])
       (type-case FAE-Value fun-val
         [(ClosureV param body env)
          (eval body (aSub param arg-val env))]
         [(ConstructorV right?)
          (VariantV right? arg-val)]
         [else (error 'eval "not callable")]))]
    [(WithType type-name var1-name var1-te var2-name var2-te body-expr)
     (eval body-expr (aSub var1-name (ConstructorV #f)
                           (aSub var2-name (ConstructorV #t) env)))]
    [(TypeCase ty dispatch-expr
               var1-name var1-id var1-rhs
               var2-name var2-id var2-rhs)
     (type-case FAE-Value (eval dispatch-expr env)
        [(VariantV right? val)
         (if (not right?)
             (eval var1-rhs (aSub var1-id val env))
             (eval var2-rhs (aSub var2-id val env)))]
        [else (error 'eval "not a variant result")])]))

;; num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

(define (num+ x y) (num-op + '+ x y))
(define (num- x y) (num-op - '- x y))
(define (num* x y) (num-op * '* x y))

(define (lookup name env)
  (type-case Env env
    [(mtSub) (error 'lookup "free variable")]
    [(aSub sub-name num rest-env)
          (if (equal? sub-name name)
              num
              (lookup name rest-env))]
    [(aRecSub sub-name val-box rest-env)
             (if (equal? sub-name name)
                 (unbox val-box)
                 (lookup name rest-env))]))

;; ----------------------------------------

(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [(mtEnv) (error 'type-lookup "free variable, so no type")]
    [(aBind name ty rest)
           (if (equal? name-to-find name)
               ty
               (type-lookup name-to-find rest))]
    [(tBind name var1-name var1-ty var2-name var2-ty rest)
     (type-lookup name-to-find rest)]))


(define (validtype ty env)
  (type-case Type ty
    [(NumT) (void)]
    [(BoolT) (void)]
    [(ArrowT a b)
     (begin
       (validtype a env)
       (validtype b env))]
    [(PairT a b)
     (begin
       (validtype a env)
       (validtype b env))]
    [(IdT id)
     (begin
       (find-type-id id env)
       (void))]))

(define (find-type-id name-to-find env)
  (type-case TypeEnv env
    [(mtEnv) (error 'find-type-id "free type name, so no type")]
    [(aBind name ty rest)
     (find-type-id name-to-find rest)]
    [(tBind name var1-name var1-ty var2-name var2-ty rest)
     (if (equal? name-to-find name)
         env
         (find-type-id name-to-find rest))]))


;; ----------------------------------------

(define (parse-type te)
  (type-case TE te
    [(NumTE) (NumT)]
    [(BoolTE) (BoolT)]
    [(PairTE  l r) (PairT (parse-type l) (parse-type r))]
    [(ArrowTE a b) (ArrowT (parse-type a)
                           (parse-type b))]
    [(IdTE name) (IdT name)]))

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
     [(Pair l r) (PairT (typecheck l env) (typecheck r env))]
    [(Fst ex)
     (type-case Type (typecheck ex env)
       [(PairT l r) l]
       [else (type-error ex "not a pair")])]
    [(Snd ex)
     (type-case Type (typecheck ex env)
       [(PairT l r) r]
       [else (type-error ex "not a pair")])]
   [(If0 test-expr then-expr else-expr)
     (let* ([test-type (type-assert (list test-expr) (NumT) env (NumT))]
            [then-type (typecheck then-expr env)]
            [else-type (type-assert (list else-expr) then-type env then-type)])
       else-type)]
    [(Rec name ty rhs-expr body-expr)
     (let* ([rhs-ty (parse-type ty)]
            [new-env (aBind name rhs-ty env)])
       (begin
         (validtype rhs-ty env)
         (type-assert (list rhs-expr) rhs-ty new-env (typecheck body-expr new-env))))]
    [(Fun name te body)
     (let* ([arg-type (parse-type te)]
            [body-type (typecheck body (aBind name arg-type env))])
       (begin
         (validtype arg-type env)
         (ArrowT arg-type body-type)))]
    [(Call fn arg)
     (type-case Type (typecheck fn env)
       [(ArrowT arg-type result-type)
        (type-assert (list arg) arg-type env result-type)]
       [else (type-error fn "function")])]
    [(WithType type-name var1-name var1-te var2-name var2-te body-expr)
     (let* ([var1-ty (parse-type var1-te)]
            [var2-ty (parse-type var2-te)]
            [new-env (tBind type-name var1-name var1-ty var2-name var2-ty env)])
       (begin
         (validtype var1-ty new-env)
         (validtype var2-ty new-env)
         (typecheck body-expr
                    (aBind var1-name
                           (ArrowT var1-ty (IdT type-name))
                           (aBind var2-name
                                  (ArrowT var2-ty (IdT type-name))
                                  new-env)))))]
    [(TypeCase type-name dispatch-expr
               var1-name var1-id var1-rhs
               var2-name var2-id var2-rhs)
     (let ([type-binding (find-type-id type-name env)]
           [expr-type (typecheck dispatch-expr env)])
       (type-case Type expr-type
         [(IdT name)
          (begin
            (unless (equal? name type-name)
              (type-error dispatch-expr (to-string type-name)))
            (matching-variant-names type-binding var1-name var2-name fae)
            (matching-variant-types type-binding var1-id var1-rhs var2-id var2-rhs env))]
           [else (type-error dispatch-expr (to-string type-name))]))]))

;; Note that both of these functions expect to be called with the
;; output of find-type-id, i.e. a tBind variant.
(define (matching-variant-types type-binding var1-id var1-rhs var2-id var2-rhs env)
  (let* ([var1-ty (tBind-var1-type type-binding)]
         [var2-ty (tBind-var2-type type-binding)]
         [env1 (aBind var1-id var1-ty env)]
         [rhs1-ty (typecheck var1-rhs env1)]
         [env2 (aBind var2-id var2-ty env)])
    (type-assert (list var2-rhs) rhs1-ty env2 rhs1-ty)))

(define (matching-variant-names type-binding var1-name var2-name fae)
  (unless
      (and (equal? var1-name (tBind-var1-name type-binding))
           (equal? var2-name (tBind-var2-name type-binding)))
    (type-error fae "matching variant names")))

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
                      (Num 0))))

;; ----------------------------------------------------------------------
(module+ test
  (print-only-errors #t)
  (test/exn (eval (Id 'x) (mtSub)) "free variable")

  (test/exn (typecheck (Id 'x) (mtEnv))  "free variable")

  (test (eval (Not (Bool #f)) (mtSub))
        (BoolV #t))

  (test/exn (typecheck (Not (Num 1)) (mtEnv)) "not bool")

  (test (typecheck (Not (Bool #f)) (mtEnv))
        (BoolT))

  (test (eval (Num 10)
                (mtSub))
        (NumV 10))
  (test (eval (Add (Num 10) (Num 17))
                (mtSub))
        (NumV 27))
  (test (eval (Sub (Num 10) (Num 7))
                (mtSub))
        (NumV 3))
  (test (eval (Call (Fun 'x (NumTE) (Add (Id 'x) (Num 12)))
                      (Add (Num 1) (Num 17)))
                (mtSub))
        (NumV 30))
  (test (eval (Id 'x)
                (aSub 'x (NumV 10) (mtSub)))
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
                (mtSub))
        (NumV 3))

  (test/exn (eval (Id 'x) (mtSub))
            "free variable")

  (test (typecheck (Num 10) (mtEnv))
        (NumT))

  (test (typecheck (Add (Num 10) (Num 17)) (mtEnv))
        (NumT))
  (test (typecheck (Sub (Num 10) (Num 7)) (mtEnv))
        (NumT))

  (test/exn (typecheck (Add (Bool #f) (Num 17)) (mtEnv)) "not num")
  (test/exn (typecheck (Sub (Bool #f) (Num 17)) (mtEnv)) "not num")
  (test/exn (typecheck (Add (Num 17) (Bool #f)) (mtEnv)) "not num")
  (test/exn (typecheck (Sub (Num 17) (Bool #f)) (mtEnv)) "not num")

  (test (typecheck (Fun 'x (NumTE) (Add (Id 'x) (Num 12))) (mtEnv))
        (ArrowT (NumT) (NumT)))

  (test/exn (typecheck (Call (Fun 'x (NumTE) (Id 'x)) (Bool #t)) (mtEnv)) "no type")

  (test (typecheck (Fun 'x (NumTE) (Fun 'y (BoolTE) (Id 'x))) (mtEnv))
        (ArrowT (NumT) (ArrowT (BoolT)  (NumT))))

  (test (typecheck (Call (Fun 'x (NumTE) (Add (Id 'x) (Num 12)))
                         (Add (Num 1) (Num 17)))
                   (mtEnv))
        (NumT))

  (test (typecheck (Fun 'x (NumTE) (Fun 'y (BoolTE) (Id 'x))) (mtEnv))
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
                   (mtEnv))
        (NumT))

  (test/exn (typecheck (Call (Num 1) (Num 2)) (mtEnv))
            "no type")

  (test/exn (typecheck (Add (Fun 'x (NumTE) (Num 12))
                            (Num 2))
                       (mtEnv))
            "no type")

  ;; Added coverage test for type-to-string
  (test/exn (typecheck (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                  (Call (Id 'f) (Num 1))) (Num 1)) (mtEnv)) "not (ArrowT (NumT) (NumT))")

  ;; Tests for if0

  (test (eval (If0 (Num 0) (Num 1) (Num 0)) (mtSub)) (NumV 1))
  (test (eval (If0 (Num 1) (Num 1) (Num 0)) (mtSub)) (NumV 0))
  (test (typecheck (If0 (Num 0) (Num 1) (Num 0)) (mtEnv)) (NumT))
  (test/exn (typecheck (If0 (Num 0) (Fun 'x (NumTE) (Id 'x)) (Num 0)) (mtEnv)) "ArrowT")
  (test/exn (typecheck (If0 (Fun 'x (NumTE) (Id 'x)) (Num 0) (Num 0)) (mtEnv)) "not num")

  ;; Tests for Rec
  (define fact-rec
    (Rec 'fact (ArrowTE (NumTE) (NumTE))
         (Fun 'n (NumTE)
              (If0 (Id 'n)
                   (Num 1)
                   (Mul (Id 'n) (Call (Id 'fact) (Sub (Id 'n) (Num 1))))))
         (Call (Id 'fact) (Num 5))))

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

  (test (typecheck fib-rec (mtEnv)) (NumT))
  (test (eval fib-rec (mtSub)) (NumV 5))
  (test (typecheck fact-rec (mtEnv)) (NumT))
  (test (eval fact-rec (mtSub)) (NumV 120))

  (test/exn (typecheck (Rec 'x (NumTE)
                            (Fun 'y (NumTE) (Num 3))
                            (Num 10))
                       (mtEnv))
          "no type")

  ;; Contrived test to get full coverage of lookup
  (test (eval (Rec 'x (NumTE)
                   (Num 10)
                   (Rec 'y (NumTE)
                        (Num 10)
                        (Id 'x)))
              (mtSub))
            (NumV 10)))


(module+ test
  (define prog1
    (WithType 'fruit 'apple (NumTE)
              'banana (ArrowTE (NumTE) (NumTE))
              (Call (Id 'apple) (Num 10))))
  (test (eval prog1 (mtSub))
        (VariantV #f (NumV 10)))

  (define prog2
    (WithType 'fruit 'apple (NumTE)
                        'banana (ArrowTE (NumTE) (NumTE))
                        (TypeCase 'fruit (Call (Id 'apple) (Num 10))
                                  'apple 'y (Add (Id 'y) (Num 1))
                                  'banana 'x (Call (Id 'x) (Num 10)))))

  (test (eval prog2 (mtSub)) (NumV 11))

  (define prog3
    (WithType 'fruit 'apple (NumTE)
              'banana (ArrowTE (NumTE) (NumTE))
              (TypeCase 'fruit (Call (Id 'banana) (Fun 'x (NumTE) (Sub (Id 'x) (Num 1))))
                                  'apple 'y (Add (Id 'y) (Num 1))
                                  'banana 'x (Call (Id 'x) (Num 10)))))
  (test (eval prog3 (mtSub)) (NumV 9))

  (test (typecheck prog1  (mtEnv)) (IdT 'fruit))

  (define prog4
    (WithType 'fruit 'apple (NumTE)
              'banana (ArrowTE (NumTE) (NumTE))
              (Fun 'x (IdTE 'fruit) (Num 10))))

  (test (typecheck prog4  (mtEnv)) (ArrowT (IdT 'fruit) (NumT)))

  (test (typecheck prog3 (mtEnv))  (NumT))

  (define prog5
    (WithType 'fruit 'apple (NumTE)
              'banana (ArrowTE (NumTE) (NumTE))
              (TypeCase 'fruit (Call (Id 'banana) (Fun 'x (NumTE) (Sub (Id 'x) (Num 1))))
                        'apple 'y (Add (Id 'y) (Num 1))
                        'banana 'x (Call (Id 'x) (Num 10)))))
  (test (typecheck prog5 (mtEnv)) (NumT))

  ;; Soundness bug from slides
  (define prog6
    (Call (WithType 'foo
                    'a (NumTE)
                    'b (NumTE)
                    (Fun 'x (IdTE 'foo)
                         (TypeCase 'foo (Id 'x)
                                   'a 'n (Id 'n)
                                   'b 'n (Id 'n))))
          (WithType 'foo
                    'c (ArrowTE (NumTE) (NumTE))
                    'd (NumTE)
                    (Call (Id 'c) (Fun 'y (NumTE) (Id 'y))))))

  ;; If we ignore types, this more or less makes sense
  (test (eval prog6 (mtSub))
        (ClosureV 'y (Id 'y) (aSub 'c (ConstructorV #f) (aSub 'd (ConstructorV #t) (mtSub)))))

  ;; This, not so much
  (test (typecheck prog6 (mtEnv)) (NumT)))

(module+ test
   (define test-pair-ex1
    (Pair (Num 1) (Bool #f)))
  (define pair-fun
    (Fun 'x (PairTE (NumTE) (NumTE)) (Fst (Id 'x))))

  (test (typecheck test-pair-ex1 (mtEnv)) (PairT (NumT) (BoolT)))
  (test (eval test-pair-ex1 (mtSub)) (PairV (NumV 1) (BoolV #f)))
  (test (typecheck (Fst test-pair-ex1) (mtEnv))  (NumT))
  (test (typecheck (Snd test-pair-ex1) (mtEnv))  (BoolT))
  (test (eval (Fst test-pair-ex1) (mtSub))  (NumV 1))
  (test/exn (eval (Fst (Fst test-pair-ex1)) (mtSub))  "not a pair")
  (test/exn (eval (Snd (Snd test-pair-ex1)) (mtSub))  "not a pair")
  (test/exn (typecheck (Fst (Fst test-pair-ex1)) (mtEnv))  "not a pair")
  (test/exn (typecheck (Snd (Snd test-pair-ex1)) (mtEnv))  "not a pair")
  (test (typecheck pair-fun (mtEnv))
        (ArrowT (PairT (NumT) (NumT)) (NumT)))
  (test/exn (typecheck (Call pair-fun (Num 1)) (mtEnv)) "not (PairT (NumT) (NumT))"))

(module+ test
  (define (list-env ex) (WithType 'numlist
                                  'empty (NumTE)
                                  'cons  (PairTE (NumTE) (IdTE 'numlist))
                                  ex))

  (test (typecheck (list-env (Num 1)) (mtEnv)) (NumT))

  (define nil (Call (Id 'empty) (Num 0)))

  (test (typecheck (list-env nil) (mtEnv)) (IdT 'numlist))

  (define (conz n l)
    (Call (Id 'cons)
          (Pair (Num n) l)))

  (test (typecheck (list-env (conz 2 (conz 1 nil))) (mtEnv)) (IdT 'numlist)))

(module+ test
  (test (eval (list-env (Id 'empty)) (mtSub)) (ConstructorV #f))
  (test (eval (list-env (Id 'cons)) (mtSub)) (ConstructorV #t)))

(module+ test
  (define (extract l)
    (list-env
     (TypeCase 'numlist l
               'empty 'n (Id 'n)
               'cons 'p (Fst (Id 'p)))))

  (test (eval (extract nil) (mtSub)) (NumV 0))
  (test (eval (extract (conz 1 nil)) (mtSub)) (NumV 1))
  (test (eval (extract (conz 2 (conz 1 nil))) (mtSub)) (NumV 2)))

(module+ test
  (test (typecheck (extract nil) (mtEnv)) (NumT))
  (test (typecheck (extract (conz 1 nil)) (mtEnv)) (NumT)))

(module+ test
  (define prog7
    (list-env
     (Rec 'len  (ArrowTE (IdTE 'numlist) (NumTE))
          (Fun 'l (IdTE 'numlist)
               (TypeCase 'numlist (Id 'l)
                         'empty 'n (Num 0)
                         'cons 'fxr (Add (Num 1) (Call (Id 'len)
                                                        (Snd (Id 'fxr))))))
          (Call (Id 'len)
                (conz 1000 (conz 100 (conz 10 (conz 1 nil))))))))

  (test (typecheck prog7 (mtEnv)) (NumT))
  (test (eval prog7 (mtSub)) (NumV 4)))

(module+ test
  (define prog8
    (list-env
     (Rec 'sum  (ArrowTE (IdTE 'numlist) (NumTE))
          (Fun 'l (IdTE 'numlist)
               (TypeCase 'numlist (Id 'l)
                         'empty 'n (Num 0)
                         'cons 'fxr (Add (Fst (Id 'fxr))
                                         (Call (Id 'sum)
                                               (Snd (Id 'fxr))))))
          (Call (Id 'sum)
                (conz 1000 (conz 100 (conz 10 (conz 1 nil))))))))

  (test (typecheck prog8 (mtEnv)) (NumT))
  (test (eval prog8 (mtSub)) (NumV 1111)))


(module+ test
  (define (run s-expr)
    (eval (parse s-expr) (mtSub)))

  (define (check s-expr)
    (typecheck (parse s-expr) (mtEnv)))

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

  (test/exn (check `{+ {fun {x : num} 12} 2}) "no type"))