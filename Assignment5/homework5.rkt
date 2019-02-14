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
  [CRef (n : Number) (id : CORE)])
  ;[With (id : Symbol) (bound-expr : BWAE) (body : BWAE)])

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


(define-type ENV
  [EmptyEnv]
  [Extend (name : Symbol) (val : Number) (rest : ENV)])

(define (lookup name env)
  (type-case ENV env
             [(EmptyEnv ) 
              (error 'lookup (string-append "no binding for " (to-string name)))]
             [(Extend id val rest-env)
              (if (eq? id name) 
                  val 
                  (lookup name rest-env))]))

;; evaluates WAE expressions by reducing them to numbers
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
(define (run sx)
  (eval (parse-sx sx) (EmptyEnv)))

(test (run `{with {x {+ 5 5}} {+ x x}}) 20)
(test (run `{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) 14)

(define (de-lookup sym lst)
  (local
    [(define
       (helper sym lst acc)
       (cond
         [(and (empty? lst)(eq? acc 0)) (parse-error sym)] ;(lookup sym (EmptyEnv))
         [(empty? lst) acc]
         [else (helper sym (rest lst) (add1 acc))]))]
    (helper sym lst 0)))

(define (de-extend sym lst)
  (local
    [(define
       (helper sym lst acc)
       (cond
         [(and (empty? lst) (eq? 0 acc)) (parse-error 'woops)] ;(lookup sym (EmptyEnv))
         [(empty? lst) (list (Extend sym acc (EmptyEnv)))]
         [else (helper sym (rest lst)
                       (foldl + 0 (cons (lookup sym (first lst)) (list acc))))]))] ;+ (lookup sym (first lst))
    (helper sym lst 0)))

;
(define (de-empty-env) (list (EmptyEnv)))
;(define-type de-bruijn
;  [de-empty-env (e : Number)])
;

(test/exn (de-lookup 'x (de-empty-env))  "undefined identifier")
(test (de-lookup 'x (de-extend 'x (de-empty-env))) 0)
(test (de-lookup 'x (de-extend 'y (de-extend 'x (de-empty-env)))) 1)
(test (de-lookup 'x (de-extend 'x (de-extend 'x (de-empty-env)))) 0)
;(test (de-lookup 'y (de-extend 'x (de-extend 'x (de-extend 'y (de-empty-env))))) 2)