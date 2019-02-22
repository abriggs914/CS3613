#lang plait

(define-type BWAE
  [Num  (n : Number)]
  [Add  (l : BWAE) (r : BWAE)]
  [Sub  (l : BWAE) (r : BWAE)]
  [Mul  (l : BWAE) (r : BWAE)]
  [Div  (l : BWAE) (r : BWAE)]
  [Id   (s : Symbol)]
  [With (id : Symbol) (bound-expr : BWAE) (body : BWAE)])


(define-type CORE
  [CNum  (n : Number)]
  [CAdd  (l : CORE) (r : CORE)]
  [CSub  (l : CORE) (r : CORE)]
  [CMul  (l : CORE) (r : CORE)]
  [CDiv  (l : CORE) (r : CORE)]
  [CRef  (ref : Number)]
  [CWith (l : CORE) (r : CORE)])

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
    [(s-exp-match? `(ANY ANY ANY) sx)
     (let* ([l (lambda () (parse-sx (sx-ref sx 1)))]
            [r (lambda () (parse-sx (sx-ref sx 2)))])
         (case (s-exp->symbol (sx-ref sx 0))
           [(+) (Add (l) (r))]
           [(-) (Sub (l) (r))]
           [(*) (Mul (l) (r))]
           [(/) (Div (l) (r))]
           [else (parse-error sx)]))]
    [else (parse-error sx)]))

(define-type-alias ENV   (Listof Number))

(define (Extend val env) (cons val env))

(define (EmptyEnv) empty)


;; evaluates BWAE expressions by reducing them to numbers
(define (eval (expr : CORE)  (env : ENV)) : Number
  (type-case CORE expr
    [(CNum n) n]
    [(CAdd l r) (+ (eval l env) (eval r env))]
    [(CSub l r) (- (eval l env) (eval r env))]
    [(CMul l r) (* (eval l env) (eval r env))]
    [(CDiv l r) (/ (eval l env) (eval r env))]
    [(CWith named-expr bound-body)
           (eval bound-body
                 (Extend (eval named-expr env) env))]
    [(CRef offset) (list-ref  env offset)]))

(module+ test
  (define test-run-env
    (Extend 1 (Extend 2 (Extend 3 (Extend 4 (EmptyEnv))))))

  (test test-run-env (list 1 2 3 4))
  (test (eval (CNum 42) test-run-env) 42)
  (test (eval (CAdd (CRef 0) (CRef 1))  test-run-env) 3)
  (test (eval (CSub (CRef 0) (CRef 1))  test-run-env) -1)
  (test (eval (CMul (CRef 2) (CRef 3))  test-run-env) 12)
  (test (eval (CDiv (CRef 2) (CRef 3))  test-run-env) 3/4)
  (test (eval (CWith (CRef 3) (CRef 0)) test-run-env) 4)
  (test (eval (CRef 0) test-run-env) 1))

(define-type-alias DE-ENV (Listof Symbol))

(define (de-lookup sym env)
  (cond
    [(empty? env) (error 'de-lookup (string-append "undefined identifier " (to-string sym)))]
    [(eq? sym (first env)) 0]
    [else (add1 (de-lookup sym (rest env)))]))

(define (de-extend s l) (cons s l))

(define (de-empty-env) empty)

(module+ test
  (test/exn (de-lookup 'x (de-empty-env))  "undefined identifier")
  (test (de-lookup 'x (de-extend 'x (de-empty-env))) 0)
  (test (de-lookup 'x (de-extend 'y (de-extend 'x (de-empty-env)))) 1)
  (test (de-lookup 'x (de-extend 'x (de-extend 'x (de-empty-env)))) 0)
  (test (de-lookup
         'y
         (de-extend 'x
                    (de-extend 'x
                               (de-extend 'y (de-empty-env))))) 2)
  (test (de-lookup
         'z
         (de-extend 'x (de-extend 'y
                                  (de-extend 'z (de-empty-env))))) 2)

  (test (de-lookup
         'z
         (de-extend 'x (de-extend 'z
                                  (de-extend 'y (de-empty-env))))) 1))


(define (preprocess [bwae-exp : BWAE] [env : DE-ENV]) : CORE
  (type-case BWAE bwae-exp
    [(Num n) (CNum n)]
    [(Add l r) (CAdd (preprocess l env) (preprocess r env))]
    [(Sub l r) (CSub (preprocess l env) (preprocess r env))]
    [(Mul l r) (CMul (preprocess l env) (preprocess r env))]
    [(Div l r) (CDiv (preprocess l env) (preprocess r env))]
    [(With bound-id named-expr bound-body)
          (CWith
           (preprocess named-expr env)
           (preprocess bound-body (de-extend bound-id env)))]
    [(Id name) (CRef (de-lookup name env))]))

(module+ test
  (define test-env (de-extend 'x (de-extend 'z (de-extend 'y (de-empty-env)))))
  (test (preprocess (Id 'x) test-env) (CRef 0))
  (test (preprocess (Add (Id 'x) (Id 'y)) test-env) (CAdd (CRef 0) (CRef 2)))
  (test (preprocess (Num 42)  test-env) (CNum 42))
  (test (preprocess (Add (Id 'x) (Id 'y)) test-env)  (CAdd (CRef 0) (CRef 2)))
  (test (preprocess (Sub (Id 'x) (Id 'z)) test-env)  (CSub (CRef 0) (CRef 1)))
  (test (preprocess (Mul (Id 'z) (Id 'z)) test-env)  (CMul (CRef 1) (CRef 1)))
  (test (preprocess (Div (Id 'y) (Id 'z)) test-env)  (CDiv (CRef 2) (CRef 1)))
  (test (preprocess (With 'x (Id 'y) (Id 'x)) test-env) (CWith (CRef 2) (CRef 0))))

;; evaluate a BWAE program contained in a s-expr
(define (run sx)
  (eval (preprocess (parse-sx sx) empty) (EmptyEnv)))

(module+ test
  (test/exn (run `{with {y} })
            "parse error")
  (test/exn (run `{+})
            "parse error")
  (test/exn (run `{& 1 2}) "parse error")

  (test (run `5) 5)
  (test (run `{+ 5 5}) 10)

  (test (run `{with {x {+ 5 5}} {+ x x}}) 20)
  (test (run `{with {x 5} {+ x x}}) 10)
  (test (run `{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) 14)
  (test (run `{with {x 5} {with {y {- x 3}} {+ y y}}}) 4)
  (test (run `{with {x 5} {with {y {- x 3}} {* y y}}}) 4)
  (test (run `{with {x 5} {with {y {- x 3}} {/ y y}}}) 1)
  (test (run `{with {x 5} {+ x {with {x 3} 10}}}) 15)
  (test (run `{with {x 5} {+ x {with {x 3} x}}}) 8)
  (test (run `{with {x 5} {+ x {with {y 3} x}}}) 10)
  (test (run `{with {x 5} {with {y x} y}}) 5)
  (test (run `{with {x 5} {with {x x} x}}) 5)
  (test/exn (run `{with {x 1} y}) "undefined identifier"))

(define minutes-spent 120)