#lang plait

;"Take what we go over for pairs and generalize it."
;"Arbitrary list of items, need to check that they are in pairs"

(define-type FAE
  [Num (n : Number)]
  [Bool (b : Boolean)]
  [Not (expr : FAE)]
  [Add (lhs : FAE)
       (rhs : FAE)]
  [Sub (lhs : FAE)
       (rhs : FAE)]
  [Id (name : Symbol)]
  [Fun (param : Symbol)
       (argty : TE)
       (body : FAE)]
  [Call (fun-expr : FAE)
       (arg-expr : FAE)]
  [Lst (id : (Listof FAE))]
  [Map (fun : FAE) (arg : FAE)]
  )

(define-type TE
  [NumTE]
  [BoolTE]
  [ArrowTE (arg : TE)
           (result : TE)]
  ;[ListTE (lst : (Listof FAE))]
  )

(define-type FAE-Value
  [NumV (n : Number)]
  [BoolV (b : Boolean)]
  [ClosureV (param : Symbol)
            (body : FAE)
            (env : Env)]
  [ListV (lst : (Listof FAE-Value))]
  )

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
  [ListT (lst : Type)]
  )

(define-type TypeEnv
  [mtEnv]
  [aBind (name : Symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

#|
ClosureV
- (Symbol FAE Env -> FAE-Value)
#<procedure:ClosureV>

> ClosureV-param
- (FAE-Value -> Symbol)
#<procedure:ClosureV-param>

> ClosureV-body
- (FAE-Value -> FAE)
#<procedure:ClosureV-body>

> ClosureV-env
- (FAE-Value -> Env)
#<procedure:ClosureV-env>

> aSub
- (Symbol FAE-Value Env -> Env)
#<procedure:aSub>
|#

;; eval : FAE Env -> FAE-Value
(define (eval a-fae env)
  (type-case FAE a-fae
    [(Num n) (NumV n)]
    [(Bool b) (BoolV b)]
    [(Not e) (BoolV (not (BoolV-b (eval e env))))]
    [(Add l r) (num+ (eval l env) (eval r env))]
    [(Sub l r) (num- (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
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
                         (ClosureV-env fun-val))))]
    [(Lst id) (ListV (map (lambda (x) (eval x env)) id))]
    ;2[(Lst id) (local
    ;2            [(define (helper lst acc)
    ;2               (cond
    ;2                 [(empty? lst) acc]
    ;2                 [else (helper (rest lst) (num+ acc (eval (first lst) env)))]))]
    ;2            (helper id (NumV 0)))]
   ;1[(Lst id) (if (empty? id) (error 'eval "not elements to infer type from")
   ;1                (ClosureV (ClosureV-param (eval (first id) env)) (first id) env))]

    [(Map fun arg) (local [(define fun-val
                             (eval fun env))
                           (define arg-val
                             (eval arg env))]
                     (eval (ClosureV-body fun-val)
                           (aSub (ClosureV-param fun-val)
                                 arg-val
                                 (ClosureV-env fun-val))))]
    ;2[(Map fun arg) (ListV
    ;2                (map (lambda (x)
    ;2                       (eval (ClosureV-body (eval fun env))
    ;2                     (aSub (ClosureV-param (eval fun env))
    ;2                           (eval x env)
    ;2                           (ClosureV-env (eval fun env))))) (list arg)))]
    ))

;; num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

;; (FAE-Value FAE-Value -> FAE-Value)
(define (num+ x y) (num-op + '+ x y))

;; (FAE-Value FAE-Value -> FAE-Value)
(define (num- x y) (num-op - '- x y))

;; (Symbol Env -> FAE-Value)
(define (lookup name env)
  (type-case Env env
    [(mtSub) (error 'lookup "free variable")]
    [(aSub Sub-name num rest-env)
          (if (equal? Sub-name name)
              num
              (lookup name rest-env))]))

;; ----------------------------------------

;; (Symbol TypeEnv -> Type)
(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [(mtEnv) (error 'type-lookup "free variable, so no type")]
    [(aBind name ty rest)
           (if (equal? name-to-find name)
               ty
               (type-lookup name-to-find rest))]))

;; ----------------------------------------
;; (TE -> Type)
(define (parse-type te)
  (type-case TE te
    [(NumTE) (NumT)]
    [(BoolTE) (BoolT)]
    [(ArrowTE a b) (ArrowT (parse-type a)
                           (parse-type b))]))

;; ('a String -> 'b)
(define (type-error fae msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string fae)
                      (string-append " not "
                                     msg)))))
;; ((Listof FAE) Type TypeEnv Type -> Type)
(define (type-assert exprs type env result) : Type
  (cond
    [(empty? exprs) result]
    [(not (equal? (typecheck (first exprs) env) type))
     (type-error (first exprs) (type-to-string type))]
    [else (type-assert (rest exprs) type env result)]))

;; (Type -> String)
(define (type-to-string [type : Type])
  (type-case Type type
    [(BoolT) "bool"]
    [(NumT) "num"]
    [else (to-string type)]))

;; (FAE TypeEnv -> Type)
(define (typecheck [fae : FAE] [env : TypeEnv]) : Type
  (type-case FAE fae
    [(Num n) (NumT)]
    [(Bool b) (BoolT)]
    [(Not e) (type-assert (list e) (BoolT) env (BoolT))]
    [(Add l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Sub l r) (type-assert (list l r) (NumT) env (NumT))]
    [(Id name) (type-lookup name env)]
    [(Fun name te body)
     (let* ([arg-type (parse-type te)]
            [body-type (typecheck body (aBind name arg-type env))])
       (ArrowT arg-type body-type))]
    [(Call fn arg)
     (type-case Type (typecheck fn env)
       [(ArrowT arg-type result-type)
        (type-assert (list arg) arg-type env result-type)]
       [else (type-error fn "function")])]
    [(Lst id)
     (cond
       [(empty? id) (error 'typecheck "not elements to infer type from")]
       [else 
     (let ([currType (typecheck (first id) env)]
       [res (map (lambda (x) (typecheck x env)) id)])     
       (if (foldl
        (lambda (x y)
           (if (equal? x currType)
               (and y #t)
               (and y #f))) #t res)
           (ListT currType)
           (error 'typecheck "not uniform element types")))])]
    [(Map fun arg)
     (let ([currType (typecheck fun env)])
     (type-case Type currType
       [(ListT lst) (first (map (lambda (x) (type-assert (list fae) (typecheck x env) env currType)) (list lst)))]
       [(ArrowT arg-type result-type)
        (type-assert (list arg) currType env result-type)]
       [else (error 'help "error here\n\n")]))]
        ;(first (map (lambda (x)
        ;       (let ([x (typecheck x env)])
        ;         (type-assert (list arg) x env x))) (list arg)))]))]
    ;1[(Map fun arg)
    ;1 (type-case Type (typecheck fun env)
    ;1   [(ArrowT arg-type result-type)
    ;1    (type-assert (list arg) arg-type env result-type)]
    ;1   [else (type-error fun "function")])]))
    ;))(list (map (lambda (x) (type-assert id x env x)) id))]))
    ))
;; ----------------------------------------

(module+ test
  ;(print-only-errors #t)
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
                                  (Call (Id 'f) (Num 1))) (Num 1)) (mtEnv))
            "not (ArrowT (NumT) (NumT))")


(test (typecheck (Lst (list (Num 1) (Num 2))) (mtEnv)) 
      (ListT (NumT)))
(test (eval (Lst (list (Num 1) (Num 2))) (mtSub))
      (ListV (list (NumV 1) (NumV 2))))
(test/exn (typecheck (Lst (list (Num 1) (Bool #t))) (mtEnv))
          "not uniform element types")
(test/exn (typecheck (Lst empty) (mtEnv))
          "not elements to infer type from")


(test (typecheck
       (Map (Fun 'x (NumTE) (Id 'x)) (Lst (list (Num 1) (Num 2)))) (mtEnv))
      (ListT (NumT)))
(test (eval
       (Map (Fun 'x (NumTE) (Id 'x)) (Lst (list (Num 1) (Num 2)))) (mtSub))
      (ListV (list (NumV 1) (NumV 2))))

  )

(define minutes-spent 60)