#lang plait
#| BNF for the PUAE language:
<PUAE> ::= <num>
| { + <PUAE> <PUAE> }
| { - <PUAE> <PUAE> }
| { * <PUAE> <PUAE> }
| { / <PUAE> <PUAE> }
| { "post" <PUAE> <PUAE> }
|#

;; PUAE abstract syntax trees
(define-type PUAE
  [Num (val : Number)]
  [Add (l : PUAE) (r : PUAE)]
  [Sub (l : PUAE) (r : PUAE)]
  [Mul (l : PUAE) (r : PUAE)]
  [Div (l : PUAE) (r : PUAE)]
  [Post (l : PostfixItem)])

;; to convert s-expressions into PUAEs
(define (parse-sx sx)
  (let ([rec (lambda (fn)
               (parse-sx (fn (s-exp->list sx))))])
    (cond
      [(s-exp-match? `NUMBER sx)
       (Num (s-exp->number sx))]
      [(s-exp-match? `(+ ANY ANY) sx)
       (Add (rec second) (rec third))]
      [(s-exp-match? `(- ANY ANY) sx)
       (Sub (rec second) (rec third))]
      [(s-exp-match? `(* ANY ANY) sx)
       (Mul (rec second) (rec third))]
      [(s-exp-match? `(/ ANY ANY) sx)
       (Div (rec second) (rec third))]
      [else (error 'parse-sx (to-string sx))])))

(define (eval expr)
  (type-case PUAE expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Post l) (eval l)]));(parse-post-item l) (parse-post-item r)]))


(define (run sx)
  (eval (parse-sx sx)))


(print-only-errors #t)
(test (run `3)  3)
(test (run `{+ 3 4})  7)
(test (run `{+ {- 3 4} 7})  6)
(test (run `{* {- 6 5} 4}) 4)
(test (run `{/ {* {- 6 5} 4} 2}) 2)
(test/exn (run `{% {- 6 5} 4}) "parse-sx: `(% (- 6 5) 4)")

(define-type PostfixItem
  [AddOp (l : PUAE) (r : PUAE)]
  [SubOp (l : PUAE) (r : PUAE)]
  [MulOp (l : PUAE) (r : PUAE)]
  [DivOp (l : PUAE) (r : PUAE)]
  [postItem (val : PUAE)])

(define (parse-post-item sx)
  (let ([rec (lambda (fn)
               (parse-sx (fn (s-exp->list sx))))])
    (cond
      [(s-exp-match? `NUMBER sx)
       (Num (s-exp->number sx))]
      [(s-exp-match? `(+ ANY ANY) sx)
       (Add (rec second) (rec third))]
      [(s-exp-match? `(- ANY ANY) sx)
       (Sub (rec second) (rec third))]
      [(s-exp-match? `(* ANY ANY) sx)
       (Mul (rec second) (rec third))]
      [(s-exp-match? `(/ ANY ANY) sx)
       (Div (rec second) (rec third))]
      [else (error 'parse-sx (to-string sx))])))

(test (run `{* {post 1 2 +} {post 3 4 +}}) 21)

(define minutes-spent 120)