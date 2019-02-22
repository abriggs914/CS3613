#lang plait

#|
 CS3613 Homework 5
 Feb.16/19
 Avery Briggs
 3741065
|#

(define-type BWAE
  [Num  (n : Number)]
  [Add  (l : BWAE) (r : BWAE)]
  [Sub  (l : BWAE) (r : BWAE)]
  [Mul  (l : BWAE) (r : BWAE)]
  [Div  (l : BWAE) (r : BWAE)]
  [With (id : Symbol) (bound-expr : BWAE) (body : BWAE)]
  [Id   (s : Symbol)])

(define-type CORE
  [CNum  (n : Number)]
  [CAdd  (l : CORE) (r : CORE)]
  [CSub  (l : CORE) (r : CORE)]
  [CMul  (l : CORE) (r : CORE)]
  [CDiv  (l : CORE) (r : CORE)]
  [CWith (l : CORE) (r : CORE)]
  [CRef (index : Number)])
  ;[With (id : Symbol) (bound-expr : BWAE) (body : BWAE)])

;('a -> 'b)
(define (parse-error sx)
  (error 'parse-sx (string-append "parse error: " (to-string sx))))

;(S-Exp Number -> S-Exp)
(define (sx-ref sx n) (list-ref (s-exp->list sx) n))

;(S-Exp -> BWAE)
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

(define-type ENV
  [EmptyEnv]
  [Extend (name : Symbol) (val : Number) (rest : ENV)]) ;(Symbol Number ENV -> ENV)

(define-type-alias ENV (Listof CORE))

;(Symbol ENV -> Number)
(define (lookup name env)
  (type-case ENV env
             [(EmptyEnv ) 
              (error 'lookup (string-append "no binding for " (to-string name)))]
             [(Extend id val rest-env)
              (if (eq? id name) 
                  val 
                  (lookup name rest-env))]))

;; evaluates WAE expressions by reducing them to numbers
;(BWAE ENV -> Number)
(define (eval expr env)
  (type-case BWAE expr
    [(Num n) n]
    [(Add l r) (+ (eval l env) (eval r env))]
    [(Sub l r) (- (eval l env) (eval r env))]
    [(Mul l r) (* (eval l env) (eval r env))]
    [(Div l r) (/ (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]))

;; evaluate a BWAE program contained in a s-expr
;(S-Exp -> Number)
(define (run sx)
  (eval (parse-sx sx) (EmptyEnv)))

(test (run `{with {x {+ 5 5}} {+ x x}}) 20)
(test (run `{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) 14)
(test (run `{with {x {- 5 1}} {with {y {- x 3}} {* x {/ y y}}}}) 4)
(test/exn (run `{with {x {- 5 1}} {with {x {- x 3}} {* y {/ y y}}}}) "lookup: no binding for 'y")
(test/exn (run `{with {x {+ 1 y}} {+ y y}}) "lookup: no binding for 'y")
(test/exn (run `{with {x {+ 1 y}} {& y y}}) "parse-sx: parse error: `(& y y)")
(test/exn (run `{with {x {+ 1 y}} {/ y y} {* 4 4}}) "parse-sx: parse error: `(with (x (+ 1 y)) (/ y y) (* 4 4))")

;('a (Listof 'b) -> Number)
(define (de-lookup sym lst)
  (local
    [(define (helper sym lst acc)
       (type-case ENV (first lst)
         [(EmptyEnv) (error 'de-lookup "undefined identifier")]
         [(Extend n v r)
          (cond
            [(equal? sym n) acc] ; return acc
            [else (helper sym (list r) (add1 acc))])]))]
    (helper sym lst 0)))


;(Symbol (Listof ENV) -> (Listof ENV))
(define (de-extend sym lst)
    (map
     (lambda (x)
       (Extend sym
               (add1
                (length lst)) x)) lst))


;(Symbol (Listof ENV) -> (Listof ENV))
(define (de-extend2 sym lst)
    (map
     (lambda (x)
       (Extend sym
               (add1
                (length lst)) x)) lst))


(define (de-empty-env) (list (EmptyEnv)))
(define-type DE-ENV
  [envR (expr : (Listof ENV))])

(test/exn (de-lookup 'x (de-empty-env))  "undefined identifier")
(test (de-lookup 'x (de-extend 'x (de-empty-env))) 0)
(test (de-lookup 'x (de-extend 'y (de-extend 'x (de-empty-env)))) 1)
(test (de-lookup 'x (de-extend 'x (de-extend 'x (de-empty-env)))) 0)
(test (de-lookup 'y (de-extend 'x (de-extend 'x (de-extend 'y (de-empty-env))))) 2)


(define test-env (envR (de-extend 'x (de-extend 'z (de-extend 'y (de-empty-env))))))

;(BWAE DE-ENV -> CORE)
(define (preprocess [bwae-exp : BWAE] [env : DE-ENV]) : CORE
  (type-case DE-ENV env
    [(envR exprENV)
     (type-case BWAE bwae-exp
       [(Num n) (CRef n)]
       [(Add l r) (CAdd (preprocess l env) (preprocess r env))]
       [(Sub l r) (CSub (preprocess l env) (preprocess r env))]
       [(Mul l r) (CMul (preprocess l env) (preprocess r env))]
       [(Div l r) (CDiv (preprocess l env) (preprocess r env))]
       [(With bound-id named-expr bound-body)
          (CWith
           (preprocess named-expr env)
           (preprocess bound-body (de-extend bound-id env)))]
       [(Id name) (CRef (de-lookup name exprENV))])]));(lookup name env)]))


(test (preprocess (Num 4) test-env) (CRef 4))
(test (preprocess (Id 'x) test-env) (CRef 0))
(test (preprocess (Add (Id 'x) (Id 'y)) test-env) (CAdd (CRef 0) (CRef 2)))
(test (preprocess (Div (Id 'x) (Id 'y)) test-env) (CDiv (CRef 0) (CRef 2)))
(test (preprocess (Mul (Id 'x) (Id 'y)) test-env) (CMul (CRef 0) (CRef 2)))
(test (preprocess (With 'x (Id 'y) (Id 'x)) test-env) (CWith (CRef 2) (CRef 0)))
(test (preprocess (Sub (With 'x (Id 'y) (Id 'x)) (With 'y (Id 'x) (Id 'y))) test-env) (CSub (CWith (CRef 2) (CRef 0)) (CWith (CRef 0) (CRef 0))))

;('a (Listof ENV) -> (Listof ENV))
(define (ExtendDB num env)
  (cons
   [(cons? (has-type env : ENV))
    (cons num env)]
   [(cons? (has-type env : (Listof Number)))
    (cons num (list))]))
    ;(type-case ENV (first env)
    ;  [(EmptyEnv) (error 'ExtendDB "error")]
    ;  [(Extend n v r) (cons num (list))])))
  ;(Symbol (Listof ENV) -> (Listof ENV))
  ;(de-extend2 (string->symbol (to-string num)) env)) ;(s-exp->symbol (number->s-exp num))

;(CORE (Listof Number) -> Number)
(define (evalC expr env)
  (type-case CORE expr
    [(CNum n) n]
    [(CAdd l r) (+ (evalC l env) (evalC r env))]
    [(CSub l r) (- (evalC l env) (evalC r env))]
    [(CMul l r) (* (evalC l env) (evalC r env))]
    [(CDiv l r) (/ (evalC l env) (evalC r env))]
    [(CWith named-expr bound-body) (list-ref env (evalC named-expr env))]
     ;(evalC bound-body
     ;      (ExtendDB (evalC named-expr env) env))]
     #|(local
       [(define (helper lst sum)
           (cond
             [(empty? lst) sum]
             [else (helper (first lst) (+ sum (list-ref env (first lst))))]))]
      (helper (map (lambda (x) (evalC bound-body
           (list-ref env x))) env) 0))]|#
    [(CRef name) (list-ref env name)]))
#|
(define-type CORE
  [CNum  (n : Number)]
  [CAdd  (l : CORE) (r : CORE)]
  [CSub  (l : CORE) (r : CORE)]
  [CMul  (l : CORE) (r : CORE)]
  [CDiv  (l : CORE) (r : CORE)]
  [CWith (l : CORE) (r : CORE)]
  [CRef (index : Number)])
|#
(define test-run-env
   (ExtendDB 1 (ExtendDB 2 (ExtendDB 3 (ExtendDB 4 (EmptyEnv))))))
(test (evalC (CAdd (CRef 0) (CRef 1))  test-run-env) 3)
(test (evalC (CWith (CRef 3) (CRef 0)) test-run-env) 4)
(test (evalC (CRef 0) test-run-env) 1)

(define minutes-spent 240)