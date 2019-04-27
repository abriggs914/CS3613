#lang plait

(define-type FAE
  [Num (n : Number)]
  [Bool (b : Boolean)]
  [Not (expr : FAE)]
  [Add (lhs : FAE)
       (rhs : FAE)]
  [Sub (lhs : FAE)
       (rhs : FAE)]
  [Lst (elements : (Listof FAE))]
  [Map (fun-exp : FAE)
       (list-expr : FAE)]
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
           (result : TE)]
  [ListTE (element-type : TE)])


(define-type FAE-Value
  [NumV (n : Number)]
  [BoolV (b : Boolean)]
  [ClosureV (param : Symbol)
            (body : FAE)
            (env : Env)]
  [ListV (elements : (Listof FAE-Value))])

(define-type Env
  [mtSub]
  [aSub (name : Symbol)
        (value : FAE-Value)
        (rest : Env)])

(define-type Type
  [NumT]
  [BoolT]
  [ArrowT (arg : Type)
          (result : Type)] 
  [ListT (element : Type)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : Symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

;; eval : FAE Env -> FAE-Value
(define (eval a-fae env)
  (local [(define (do-call fun-val arg-val)
            (eval (ClosureV-body fun-val)
                  (aSub (ClosureV-param fun-val)
                        arg-val
                        (ClosureV-env fun-val))))]
    (type-case FAE a-fae
      [(Num n) (NumV n)]
      [(Bool b) (BoolV b)]
      [(Lst elements) (ListV (map (lambda (thing) (eval thing env)) elements))]
      [(Not e) (BoolV (not (BoolV-b (eval e env))))]
      [(Add l r) (Num+ (eval l env) (eval r env))]
      [(Sub l r) (Num- (eval l env) (eval r env))]
      [(Id name) (lookup name env)]
      [(Fun param arg-te body-expr)
       (ClosureV param body-expr env)]
      [(Map fun-expr list-expr)
       (let* ([list-val (eval list-expr env)]
              [elements (ListV-elements list-val)])
         (ListV (map (lambda (thing)
                       (do-call (eval fun-expr env) thing))
                     elements)))]
      [(Call fun-expr arg-expr)
       (do-call (eval fun-expr env) (eval arg-expr env))])))

;; Num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (Num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

(define (Num+ x y) (Num-op + '+ x y))
(define (Num- x y) (Num-op - '- x y))

(define (lookup name env)
  (type-case Env env
    [(mtSub) (error 'lookup "free variable")]
    [(aSub Sub-name Num rest-env)
     (if (equal? Sub-name name)
         Num
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
    [(ListTE e) (ListT (parse-type e))]
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
    [(BoolT) "Bool"]
    [(NumT) "Num"]
    [else (to-string type)]))

(define (typecheck [fae : FAE] [env : TypeEnv]) : Type
  (type-case FAE fae
    [(Num n) (NumT)]
    [(Bool b) (BoolT)]
    [(Not e) (type-assert (list e) (BoolT) env (BoolT))]
    [(Add l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Sub l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Id name) (type-lookup name env)]
    [(Lst elements)
     (local [(define (check-list-type types)
               (cond
                 [(empty? types) (type-error fae "elements to infer type from")]
                 [(eq? (length types) 1) (ListT (first types))]
                 [(equal? (first types) (second types))
                  (check-list-type (rest types))]
                 [else (type-error fae "uniform element types")]))]
       (check-list-type
        (map (lambda (thing)
               (typecheck thing env))
             elements)))]
    [(Map fn arg)
     (type-case Type (typecheck fn env)
       [(ArrowT from-type to-type)
        (type-case Type (typecheck arg env)
          [(ListT element-type)
           (if (equal? from-type element-type)
               (ListT to-type)
               (type-error fn "correct argument type"))]
          [else (type-error arg "list")])]
       [else (type-error fn "function")])]
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

  (test/exn (typecheck (Not (Num 1)) (mtEnv)) "not Bool")
  
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

  (test/exn (typecheck (Add (Bool #f) (Num 17)) (mtEnv)) "not Num")
  (test/exn (typecheck (Sub (Bool #f) (Num 17)) (mtEnv)) "not Num")
  (test/exn (typecheck (Add (Num 17) (Bool #f)) (mtEnv)) "not Num")
  (test/exn (typecheck (Sub (Num 17) (Bool #f)) (mtEnv)) "not Num")

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

  ;; Added tests for Lst and Map
  (test (typecheck (Lst (list (Num 1) (Num 2))) (mtEnv)) (ListT (NumT)))
  (test (eval (Lst (list (Num 1) (Num 2))) (mtSub))
        (ListV (list (NumV 1) (NumV 2))))
  (test/exn (typecheck (Lst (list (Num 1) (Bool #t))) (mtEnv)) "not uniform element types")

  (test (eval (Lst (list (Bool #f) (Bool #t))) (mtSub))
        (ListV (list (BoolV #f) (BoolV #t))))


  (test (typecheck (Fun 'x (NumTE) (Lst (list (Id 'x)))) (mtEnv))
        (ArrowT (NumT) (ListT (NumT))))

  (test (typecheck (Fun 'x (ListTE (NumTE)) (Num 1)) (mtEnv))
        (ArrowT (ListT (NumT)) (NumT)))

  (test/exn (typecheck (Lst empty) (mtEnv)) "not elements to infer type from")

  (test (typecheck
         (Map (Fun 'x (NumTE) (Id 'x)) (Lst (list (Num 1) (Num 2)))) (mtEnv))
        (ListT (NumT)))

  (test (eval
         (Map (Fun 'x (NumTE) (Id 'x)) (Lst (list (Num 1) (Num 2)))) (mtSub))
        (ListV (list (NumV 1) (NumV 2))))

  (test (eval
         (Map (Fun 'x (BoolTE) (Id 'x)) (Lst (list (Bool #f) (Bool #t)))) (mtSub))
        (ListV (list (BoolV #f) (BoolV #t))))

  (test/exn
   (typecheck
    (Map (Fun 'x (NumTE) (Id 'x)) (Lst (list (Bool #f) (Bool #t)))) (mtEnv)) "no type")

  (test/exn
   (typecheck
    (Map (Num 1) (Lst (list (Bool #f) (Bool #t)))) (mtEnv)) "no type")

  (test/exn
   (typecheck
    (Map (Fun 'x (NumTE) (Id 'x)) (Bool #t)) (mtEnv)) "no type"))

(define minutes-spent 120)