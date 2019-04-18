#lang plait

(define-type FAE
  [Num (n : Number)]
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
  [If0 (test-expr : FAE)
       (then-expr : FAE)
       (else-expr : FAE)]
  [Rec (name : Symbol)
    (ty : TE)
    (rhs-expr : FAE)
    (body-expr : FAE)]
  [With (bound-id : Symbol)
        (bound-body : FAE)
        (bound-expr : FAE)])

(define-type TE
  [NumTE]
  [BoolTE]
  [ArrowTE (arg : TE)
           (result : TE)]
  [GuessTE])

(define-type FAE-Value
  [NumV (n : Number)]
  [ClosureV (param : Symbol)
            (body : FAE)
            (ds : DefrdSub)])

(define-type DefrdSub
  [mtSub]
  [aSub (name : Symbol)
        (value : FAE-Value)
        (rest : DefrdSub)]
  [aRecSub (name : Symbol)
           (value-box : (Boxof FAE-Value))
           (rest : DefrdSub)])

(define-type Type
  [NumT]
  [BoolT]
  [ArrowT (arg : Type)
          (result : Type)]
  [VarT (is : (Boxof (Optionof Type)))])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : Symbol)
         (type : Type)
         (rest : TypeEnv)])

;; ----------------------------------------

;; eval : FAE DefrdSub -> FAE-Value
(define (eval a-fae ds)
  (type-case FAE a-fae
    [(Num n) (NumV n)]
    [(Add l r) (num+ (eval l ds) (eval r ds))]
    [(Sub l r) (num- (eval l ds) (eval r ds))]
    [(Id name) (lookup name ds)]
    [(Fun param arg-te body-expr)
         (ClosureV param body-expr ds)]
    [(Call fun-expr arg-expr)
         (local [(define fun-val
                   (eval fun-expr ds))
                 (define arg-val
                   (eval arg-expr ds))]
           (eval (ClosureV-body fun-val)
                   (aSub (ClosureV-param fun-val)
                         arg-val
                         (ClosureV-ds fun-val))))]
    [(If0 test-expr then-expr else-expr)
         (if (numzero? (eval test-expr ds))
             (eval then-expr ds)
             (eval else-expr ds))]
    [(Rec bound-id type named-expr body-expr)
      (local [(define value-holder (box (NumV 42)))
              (define new-ds (aRecSub bound-id
                                      value-holder
                                      ds))]
        (begin
          (set-box! value-holder (eval named-expr new-ds))
          (eval body-expr new-ds)))]
    [(With id expr body) (eval body (aSub id (eval expr ds) ds))]))


;; num-op : (Number Number -> Number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name x y)
  (NumV (op (NumV-n x) (NumV-n y))))

(define (num+ x y) (num-op + '+ x y))
(define (num- x y) (num-op - '- x y))

(define (numzero? x) (= 0 (NumV-n x)))

(define (lookup name ds)
  (type-case DefrdSub ds
    [(mtSub) (error 'lookup "free variable")]
    [(aSub sub-name val rest-ds)
          (if (symbol=? sub-name name)
              val
              (lookup name rest-ds))]
    [(aRecSub sub-name val-box rest-ds)
             (if (symbol=? sub-name name)
                 (unbox val-box)
                 (lookup name rest-ds))]))


;; ----------------------------------------

(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [(mtEnv ) (error 'type-lookup "free variable, so no type")]
    [(aBind name ty rest)
           (if (symbol=? name-to-find name)
               ty
               (type-lookup name-to-find rest))]))

;; ----------------------------------------

(define (parse-type te)
  (type-case TE te
    [(NumTE) (NumT)]
    [(BoolTE) (BoolT)]
    [(ArrowTE a b) (ArrowT (parse-type a)
                           (parse-type b))]
    [(GuessTE) (VarT (box (none)))]))

; (Type -> Type)
(define (resolve t)
  (type-case Type t
    [(VarT is)
     (type-case (Optionof Type) (unbox is)
       [(none) t]
       [(some t2) (resolve t2)])]
    [else t]))

;(Type Type -> Boolean)
(define (occurs? r t)
  (type-case Type t
    [(NumT) #f]
    [(BoolT) #f]
    [(ArrowT a b)
     (or (occurs? r a)
         (occurs? r b))]
    [(VarT is)
     (or (equal? r t)
         (type-case (Optionof Type) (unbox is)
           [(none) #f]
           [(some t2) (occurs? r t2)]))]))

(define (type-error fae t1 t2)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string fae)
                      (string-append
                       " type "
                       (string-append
                        (to-string t1)
                        (string-append
                         " vs. "
                         (to-string t2))))))))

;((Boxof (Optionof Type)) Type 'a -> Void)
(define (unify-type-var! T tau2 expr)
  (type-case (Optionof Type) (unbox T)
    [(some t3) (unify! t3 tau2 expr)]
    [(none)
     (let ([t3 (resolve tau2)]
           [Tv (VarT T)])
       (cond
         [(equal? Tv t3) (void)]
         [(occurs? Tv t3) (type-error expr Tv t3)]
         [else (set-box! T (some t3))]))]))

;('a 'a 'b -> Void)
(define (unify-assert! tau type-val expr)
  (unless (equal? tau type-val)
    (type-error expr tau type-val)))

;(Type Type 'a -> Void)
(define (unify! t1 t2 expr)
  (type-case Type t1
    [(VarT is1) (unify-type-var! is1 t2 expr)]
    [else
     (type-case Type t2
       [(VarT is2) (unify-type-var! is2 t1 expr)]
       [(NumT) (unify-assert! t1 (NumT) expr)]
       [(BoolT) (unify-assert! t1 (BoolT) expr)]
       [(ArrowT a2 b2)
        (type-case Type t1
          [(ArrowT a1 b1)
           (begin
             (unify! a1 a2 expr)
             (unify! b1 b2 expr))]
          [else (type-error expr t1 t2)])])]))

;(FAE TypeEnv -> Type)
(define (typecheck [fae : FAE] [env : TypeEnv]) : Type
  (type-case FAE fae
    [(Num n) (NumT)]
    [(Add l r) (begin
                 (unify! (typecheck l env) (NumT) l)
                 (unify! (typecheck r env) (NumT) r)
                 (NumT))]
    [(Sub l r) (begin
                 (unify! (typecheck l env) (NumT) l)
                 (unify! (typecheck r env) (NumT) r)
                 (NumT))]
    [(Id name) (type-lookup name env)]
    [(Fun name te body)
     (let* ([arg-type (parse-type te)]
            [res-type (typecheck body (aBind name arg-type env))])
       (ArrowT arg-type res-type))]
    [(Call fn arg)
     (let ([r-type (VarT (box (none)))]
           [a-type (typecheck arg env)]
           [fn-type (typecheck fn env)])
       (begin
         (unify! (ArrowT a-type r-type) fn-type fn)
         r-type))]
    [(If0 test-expr then-expr else-expr)
     (let ([test-ty (typecheck test-expr env)]
           [then-ty (typecheck then-expr env)]
           [else-ty (typecheck else-expr env)])
       (begin
         (unify! test-ty (NumT) test-expr)
         (unify! then-ty else-ty else-expr)
         then-ty))]
    [(Rec name ty rhs-expr body-expr)
     (let* ([type-ann (parse-type ty)]
            [new-env (aBind name type-ann env)]
            [rhs-ty (typecheck rhs-expr new-env)])
       (begin
         (unify! type-ann rhs-ty rhs-expr)
         (typecheck body-expr new-env)))]
    [(With id bound-exp body)
     (let* ([id-type (aSub id  env)]
            [new-env env])
       (typecheck body env))]))

;; ----------------------------------------

(module+ test
  (print-only-errors #t)
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

  (test (eval (If0 (Num 0) (Num 1) (Num 2))
              (mtSub))
        (NumV 1))
  (test (eval (If0 (Num 1) (Num 1) (Num 2))
              (mtSub))
        (NumV 2))
  (test (eval (Rec 'a (NumTE)
                   (Num 10)
                   (Add (Id 'a) (Num 1)))
              (mtSub))
        (NumV 11))
  (test (eval (Rec 'fib (ArrowTE (NumTE) (NumTE))
                   (Fun 'x (NumTE)
                        (If0 (Id 'x)
                             (Num 1)
                             (If0 (Sub (Id 'x) (Num 1))
                                  (Num 1)
                                  (Add (Call (Id 'fib) (Sub (Id 'x) (Num 1)))
                                       (Call (Id 'fib) (Sub (Id 'x) (Num 2)))))))
                   (Call (Id 'fib) (Num 4)))
              (mtSub))
        (NumV 5))


  (test/exn (eval (Id 'x) (mtSub))
            "free variable")

  (test (unify! (typecheck (Num 10) (mtEnv))
                (NumT)
                (Num -1))
        (void))

  (test (unify! (typecheck (Add (Num 10) (Num 17)) (mtEnv))
                (NumT)
                (Num -1))
        (void))
  (test (unify! (typecheck (Sub (Num 10) (Num 7)) (mtEnv))
                (NumT)
                (Num -1))
        (void))

  (test (unify! (typecheck (Fun 'x (NumTE) (Add (Id 'x) (Num 12))) (mtEnv))
                (ArrowT (NumT) (NumT))
                (Num -1))
        (void))

  (test (unify! (typecheck (Fun 'x (NumTE) (Fun 'y (BoolTE) (Id 'x))) (mtEnv))
                (ArrowT (NumT) (ArrowT (BoolT)  (NumT)))
                (Num -1))
        (void))

  (test (unify! (typecheck (Call (Fun 'x (NumTE) (Add (Id 'x) (Num 12)))
                                 (Add (Num 1) (Num 17)))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))

  (test (unify! (typecheck (Call (Fun 'x (GuessTE) (Add (Id 'x) (Num 12)))
                                 (Add (Num 1) (Num 17)))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))

  ;; illustrate that the return of our typecheck function can be a bit messy
  (test (typecheck (Call (Fun 'x (GuessTE) (Add (Id 'x) (Num 12)))
                                 (Add (Num 1) (Num 17)))
                           (mtEnv))
        (VarT (box (some (NumT)))))

  (test (unify! (typecheck (Fun 'x (GuessTE) (Add (Id 'x) (Num 12)))
                           (mtEnv))
                (ArrowT (NumT) (NumT))
                (Num -1))
        (void))

  (test (unify! (typecheck (Fun 'x (GuessTE) (If0 (Num 0) (Id 'x) (Id 'x)))
                           (mtEnv))
                (ArrowT (NumT) (NumT))
                (Num -1))
        (void))

  (test (unify! (typecheck (Call (Fun 'x (NumTE)
                                      (Call (Fun 'f (ArrowTE (NumTE) (NumTE))
                                                 (Add (Call (Id 'f) (Num 1))
                                                      (Call (Fun 'x (NumTE) (Call (Id 'f) (Num 2)))
                                                            (Num 3))))
                                            (Fun 'y (NumTE)
                                                 (Add (Id 'x)
                                                      (Id 'y)))))
                                 (Num 0))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))

  (test (unify! (typecheck (If0 (Num 0) (Num 1) (Num 2))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))
  (test (unify! (typecheck (If0 (Num 0) 
                                (Fun 'x (NumTE) (Id 'x))
                                (Fun 'y (NumTE) (Num 3)))
                           (mtEnv))
                (ArrowT (NumT) (NumT))
                (Num -1))
        (void))
  (test (unify! (typecheck (Rec 'a (NumTE)
                                (Num 10)
                                (Add (Id 'a) (Num 1)))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))
  (test (unify! (typecheck (Rec 'fib (ArrowTE (NumTE) (NumTE))
                                (Fun 'x (NumTE)
                                     (If0 (Id 'x)
                                          (Num 1)
                                          (If0 (Sub (Id 'x) (Num 1))
                                               (Num 1)
                                               (Add (Call (Id 'fib) (Sub (Id 'x) (Num 1)))
                                                    (Call (Id 'fib) (Sub (Id 'x) (Num 2)))))))
                                (Call (Id 'fib) (Num 4)))
                           (mtEnv))
                (NumT)
                (Num -1))
        (void))


  (test/exn (typecheck (Call (Num 1) (Num 2)) (mtEnv))
            "no type")

  (test/exn (typecheck (Add (Fun 'x (NumTE) (Num 12))
                            (Num 2))
                       (mtEnv))
            "no type")
  (test/exn (typecheck (If0 (Num 0) 
                            (Num 7)
                            (Fun 'y (NumTE) (Num 3)))
                       (mtEnv))
            "no type")
  (test/exn (typecheck (Rec 'x (NumTE)
                            (Fun 'y (NumTE) (Num 3))
                            (Num 10))
                       (mtEnv))
            "no type")
  (test/exn (typecheck (Rec 'x (ArrowTE (NumTE) (NumTE))
                            (Fun 'y (NumTE) (Num 3))
                            (Add (Num 1) (Id 'x)))
                       (mtEnv))
            "no type")

  (test/exn (unify! (typecheck (Fun 'x (GuessTE) (Add (Id 'x) (Num 12)))
                               (mtEnv))
                    (ArrowT (BoolT) (NumT))
                    (Num -1))
            "no type")

  (test/exn (typecheck (Fun 'x (GuessTE) (Call (Id 'x) (Id 'x)))
                       (mtEnv))
            "no type")

  ;; soundness bug still exists
  #;(test/exn (typecheck (Rec 'f (ArrowTE (NumTE) (NumTE)) (Id 'f) (Call (Id 'f) (Num 10)))
                       (mtEnv))
            "no type")

   (test (eval
         (With
          'add3
          (Fun 'x (GuessTE) (Add (Id 'x) (Num 3)))
          (With
           'add1
           (Fun 'x (NumTE) (Add (Id 'x) (Num 1)))
           (With 'x (Num 3) (Call (Id 'add1) (Call (Id 'add3) (Id 'x))))))
         (mtSub))
        (NumV 7))

  (test (eval
         (With 'identity (Fun 'x (GuessTE) (Id 'x))
          (With 'foo (Fun 'x (NumTE) (Add (Id 'x) (Num 1)))
                (Call (Call (Id 'identity) (Id 'foo)) (Num 123))))
         (mtSub))
        (NumV 124))

  (test (eval (With 'x  (Num 3)
                    (With
                     'f (Fun 'y (GuessTE) (Add (Id 'x) (Id 'y)))
                     (With 'x (Num 5) (Call (Id 'f) (Num 4)))))
              (mtSub))
        (NumV 7))

  (test (eval
         (Call (With 'x (Num 3) (Fun 'y (GuessTE) (Add (Id 'x) (Id 'y)))) (Num 4))
         (mtSub))
        (NumV 7))

  (test
   (resolve
    (typecheck
     (With
      'add3
      (Fun 'x (GuessTE) (Add (Id 'x) (Num 3)))
      (With
       'add1
       (Fun 'x (NumTE) (Add (Id 'x) (Num 1)))
       (With 'x (Num 3) (Call (Id 'add1) (Call (Id 'add3) (Id 'x))))))
     (mtEnv)))
   (NumT))

  (test
   (resolve
    (typecheck
     (With 'identity (Fun 'x (GuessTE) (Id 'x))
           (With 'foo (Fun 'x (NumTE) (Add (Id 'x) (Num 1)))
                 (Call (Call (Id 'identity) (Id 'foo)) (Num 123))))
     (mtEnv)))
   (NumT))

  (test
   (resolve
    (typecheck (With 'x  (Num 3)
                     (With
                      'f (Fun 'y (GuessTE) (Add (Id 'x) (Id 'y)))
                      (With 'x (Num 5) (Call (Id 'f) (Num 4)))))
               (mtEnv)))
   (NumT))

  (test
   (resolve
    (typecheck
     (Call (With 'x (Num 3) (Fun 'y (GuessTE) (Add (Id 'x) (Id 'y)))) (Num 4))
     (mtEnv)))
   (NumT)))