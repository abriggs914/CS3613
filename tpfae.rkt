#lang plait

(define-type TPRFAE
  [Num (n : Number)]
  [Bool (b : Boolean)]
  [Not (expr : TPRFAE)]
  [Add (lhs : TPRFAE)
       (rhs : TPRFAE)]
  [Sub (lhs : TPRFAE)
       (rhs : TPRFAE)]
  [Id (name : Symbol)]
  [Pair (left : TPRFAE)  (right : TPRFAE)]
  [Fst (v : TPRFAE)]
  [Snd (v : TPRFAE)]
  [Fun (param : Symbol)
       (argty : TE)
       (body : TPRFAE)]
  [Call (fun-expr : TPRFAE)
        (arg-expr : TPRFAE)])

(define-type TE
  [NumTE]
  [BoolTE]
  [PairTE (left : TE)
          (right : TE)]
  [ArrowTE (arg : TE)
           (result : TE)])

(define-type FAE-Value
  [NumV (n : Number)]
  [BoolV (b : Boolean)]
  [PairV (l : FAE-Value) (r : FAE-Value)]
  [ClosureV (param : Symbol)
            (body : TPRFAE)
            (env : Env)])

(define-type Env
  [mtSub]
  [aSub (name : Symbol)
        (value : FAE-Value)
        (rest : Env)])

(define-type Type
  [NumT]
  [BoolT]
  [IdT (name : Symbol)]
  [PairT (left : Type) (right : Type)]
  [ArrowT (arg : Type)
          (result : Type)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : Symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

;; eval : TPRFAE Env -> FAE-Value
(define (eval a-fae env)
  (type-case TPRFAE a-fae
    [(Num n) (NumV n)]
    [(Bool b) (BoolV b)]
    [(Not e) (BoolV (not (BoolV-b (eval e env))))]
    [(Add l r) (num+ (eval l env) (eval r env))]
    [(Sub l r) (num- (eval l env) (eval r env))]
    [(Pair l r) (PairV (eval l env) (eval r env))]    
    [(Fst ex)
     (type-case FAE-Value (eval ex env)
       [(PairV l r) l]
       [else (type-error 'eval "not a pair")])]
    [(Snd ex)
     (type-case FAE-Value (eval ex env)
       [(PairV l r) r]
       [else (type-error 'eval "not a pair")])]    [(Id name) (lookup name env)]
    [(Fun param arg-te body-expr)
     (ClosureV param body-expr env)]
    [(Call fun-expr arg-expr)
     (local [(define fun-val
               (eval fun-expr env))
             (define arg-val
               (eval arg-expr env))]
       (eval (ClosureV-body fun-val)
             (aSub (ClosureV-param fun-val)
                   arg-val
                   (ClosureV-env fun-val))))]))

;; num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

(define (num+ x y) (num-op + '+ x y))
(define (num- x y) (num-op - '- x y))

(define (lookup name env)
  (type-case Env env
    [(mtSub) (error 'lookup "free variable")]
    [(aSub Sub-name num rest-env)
     (if (equal? Sub-name name)
         num
         (lookup name rest-env))]))

;; ----------------------------------------

(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [(mtEnv) (error 'type-lookup "free variable, so no type")]
    [(aBind name ty rest)
     (if (equal? name-to-find name)
         ty
         (type-lookup name-to-find rest))]))

;; ----------------------------------------

(define (parse-type te)
  (type-case TE te
    [(NumTE) (NumT)]
    [(BoolTE) (BoolT)]
    [(ArrowTE a b) (ArrowT (parse-type a)
                           (parse-type b))]
    [(PairTE  l r) (PairT (parse-type l) (parse-type r))]))

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

(define (typecheck [fae : TPRFAE] [env : TypeEnv]) : Type
  (type-case TPRFAE fae
    [(Num n) (NumT)]
    [(Bool b) (BoolT)]
    [(Not e) (type-assert (list e) (BoolT) env (BoolT))]
    [(Add l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Sub l r) (type-assert (list l r) (NumT) env (NumT))]
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
    [(Fun name te body)
     (let* ([arg-type (parse-type te)]
            [body-type (typecheck body (aBind name arg-type env))])
       (ArrowT arg-type body-type))]
    [(Call fn arg)
     (type-case Type (typecheck fn env)
       [(ArrowT arg-type result-type)
        (type-assert (list arg) arg-type env result-type)]
       [else (type-error fn "function")])]))

;; ----------------------------------------

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

  ;; Coverage tests for pairs
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