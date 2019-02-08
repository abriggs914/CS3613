#lang plait

#|
  CS3613 Assingment 3
  Feb.1/19
  Avery Briggs
  3471065
|#

#| BNF for the PUAE language:
<PUAE> ::= <num>
| { + <PUAE> <PUAE> }
| { - <PUAE> <PUAE> }
| { * <PUAE> <PUAE> }
| { / <PUAE> <PUAE> }
| {post <PostfixItem> <PostfixItem> }
| {postItem <PostfixItem> <PUAE> }
|#

;; PUAE abstract syntax trees
(define-type PUAE
  [Num (val : Number)] ;(Number -> PUAE)
  [Add (l : PUAE) (r : PUAE)] ;(PUAE PUAE -> PUAE)
  [Sub (l : PUAE) (r : PUAE)] ;(PUAE PUAE -> PUAE)
  [Mul (l : PUAE) (r : PUAE)] ;(PUAE PUAE -> PUAE)
  [Div (l : PUAE) (r : PUAE)] ;(PUAE PUAE -> PUAE)
  [Post (p : PostfixItem)])


(define-type PostfixItem
  [AddOp (l : PUAE) (r : PUAE)]
  [SubOp (l : PUAE) (r : PUAE)]
  [MulOp (l : PUAE) (r : PUAE)]
  [DivOp (l : PUAE) (r : PUAE)]
  [postItem (l : PUAE) (r : PUAE)]
  [Expr (e : PostfixItem)])

;; to convert s-expressions into PUAEs
;(S-Exp -> PUAE)
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
      [(s-exp-match? `(post ANY ANY ANY) sx)
       (parse-post-item sx)]
      [else (error 'parse-sx (to-string sx))])))

(define (parse-post-item sx)
  (let ([rec (lambda (fn)
               (parse-post-item (fn (s-exp->list sx))))])
    (cond
      [(s-exp-match? `NUMBER sx)
       (Num (s-exp->number sx))]
      [(s-exp-match? `(post ANY ANY +) sx)
       (Add (rec second) (rec third))]
      [(s-exp-match? `(post ANY ANY -) sx)
       (Sub (rec second) (rec third))]
      [(s-exp-match? `(post ANY ANY *) sx)
       (Mul (rec second) (rec third))]
      [(s-exp-match? `(post ANY ANY /) sx)
       (Div (rec second) (rec third))]
      [else (error 'parse-sx (to-string sx))])))

;(PUAE -> Number)
(define (eval expr)
  (type-case PUAE expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Post p) (post-eval (list p) (list))]))

; I had trouble with this function, I wasn't entirely sure what to do with the
; odd input cases: `{* {+ {post 1} {post 2}} {+ {post 3} {post 4}}}.
(define (post-eval items stack)
  (cond
    ;; If no operations, return the unique value on the stack.
    [(empty? items) (first stack)]
    ;; Otherwise, there's an operation to perform
    [else
     (local [(define (pop2-and-apply op)
               (cond
                 [(or (odd? (length stack)) (empty? stack)) (error 'parse-sx (to-string op))]
                 [else (op (first stack) (first (rest stack)))]))]
       (type-case PostfixItem (first items)
         [(AddOp l r) (pop2-and-apply +)]
         [(SubOp l r) (pop2-and-apply -)]
         [(MulOp l r) (pop2-and-apply *)]
         [(DivOp l r) (pop2-and-apply /)]
         [(postItem l r) (pop2-and-apply -)]
         [(Expr exp) (post-eval (list exp) stack)]))]))

;(S-Exp -> Number)
(define (run sx)
  (eval (parse-sx sx)))
#|
(test (run `3)  3)
(test (run `{+ 3 4})  7)
(test (run `{+ {- 3 4} 7}) 6)
(test (run `{/ {* {- 6 5} 4} 2}) 2)
(test/exn (run `{% {- 6 5} 4}) "parse-sx: `(% (- 6 5) 4)")
(test (run `{- {post 15 2 *} {post 3 4 *}}) 18)
(test (run `{- {post 30 2 /} {post 3 4 -}}) 16)
(test (run `{* {post 1 2 +} {post 3 4 +}}) 21)
(test (run `{post {post 1 2 +} {post 3 4 +} *}) 21)
; Ihad trouble getting these tests to pass
(test (run `{post 1 2 + {+ 3 4} *}) 21)
(test (run `{* {+ {post 1} {post 2}} {+ {post 3} {post 4}}}) 21)


; These tests will be successfully turned into PUAE's but will fail when evaluated
(test/exn (run `{post 1 + *}) "parse-sx: `+")
(test/exn (run `{post 1 2 3}) "parse-sx: `(post 1 2 3)")
(test/exn (run `{post 1 2 3 +}) "parse-sx: `(post 1 2 3 +)")
(test/exn (run `{post * * *}) "parse-sx: `*")
(test/exn (run `{post + 1 2}) "parse-sx: `(post + 1 2)")
(test/exn (run `{post 1 + 9}) "parse-sx: `(post 1 + 9)")
(test/exn (run `{post}) "parse-sx: `(post)")

(define x `(post 1 2 +))
(define y (parse-post-item x))
(define z (AddOp y y))
(define a (Post z))
|#
(module+ test
  (print-only-errors #t)
  ;; original tests

  (test (run `3)  3)
  (test (run `{+ 3 4})  7)
  (test (run `{+ {- 3 4} 7})  6)

  ;; Full coverage for original code
  (test (run `{* 1 2}) 2)
  (test (run `{/ 1 2}) 1/2)
  (test/exn (run `blah) "blah")

  ;; Test for coverage of updated parser, excluding post part.
  (test/exn (run `{bleh}) "parse error")

  ;; tests for expressions that parse fine, but throw a runtime error
  ;; these also provide coverage for the postfix part of the parser
  (test/exn (run `{post 1 + *})    "insufficient stack values")
  (test/exn (run `{post 1 -})    "insufficient stack values")
  (test/exn (run `{post 1 /})    "insufficient stack values")
  (test/exn (run `{post 1 2 3})    "too many values left")
  (test/exn (run `{post 1 2 3 +})  "too many values left")
  (test/exn (run `{post * * *})    "insufficient stack values")
  (test/exn (run `{post + 1 2})    "insufficient stack values")
  (test/exn (run `{post 1 + 9})    "insufficient stack values")
  (test/exn (run `{post})          "no value on the stack")
  (test/exn (run `{post 1 &})    "bad postfix operator")


  ;; more tests for the new post functionality, including expressions
  ;; given in the HW. After this group of tests, there should be full
  ;; coverage.
  (test (run `{post 2 3 +})  5)
  (test (run `{post 1 2 + 3 4 + *})  21)
  (test (run `{* {post 1 2 +} {post 3 4 +}})  21)
  (test (run `{post 1 2 + {+ 3 4} *})  21)
  (test (run `{post {post 1 2 +} {post 3 4 +} *})  21)
  (test (run `{* {+ {post 1} {post 2}} {+ {post 3} {post 4}}})  21)

  ;; additional tests for non-commutative operators
  (test (run `{post 3 1 -})  2)
  (test (run `{post 6 3 /})  2))


(define minutes-spent 120)
