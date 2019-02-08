#lang plait

#| BNF for the PUAE language:
     puae: NUMBER
              | { + puae puae }
              | { - puae puae }
              | { * puae puae }
              | { / puae puae }
              | { "post" post+ }
     post: puae | + | - | * | /
|#

;; PUAE abstract syntax trees
(define-type PUAE
  [Num  (value : Number)]
  [Add  (left : PUAE) (right : PUAE)]
  [Sub  (left : PUAE) (right : PUAE)]
  [Mul  (left : PUAE) (right : PUAE)]
  [Div  (left : PUAE) (right : PUAE)]
  [Post (items : (Listof PostfixItem))])

(define-type PostfixItem
  [Expr (expr : PUAE)]
  [AddOp]
  [SubOp]
  [MulOp]
  [DivOp])


;; to convert s-expressions into PUAEs
(define (parse-sx sexpr)
  (let* ([sx-ref (lambda (n) (list-ref (s-exp->list sexpr) n))]
         [l (lambda () (parse-sx (sx-ref 1)))]
         [r (lambda () (parse-sx (sx-ref 2)))]
         [parse-error (lambda ()
                        (error 'parse-sx (string-append "parse error: " (to-string sexpr))))])
  (cond
    [(s-exp-match? `NUMBER sexpr) (Num (s-exp->number sexpr))]
    [(s-exp-match? `(post ANY ...) sexpr)
     (Post (map parse-post-item (rest (s-exp->list sexpr))))]
    [(s-exp-list? sexpr)
     (case (s-exp->symbol (sx-ref 0))
       [(+) (Add (l) (r))]
       [(-) (Sub (l) (r))]
       [(*) (Mul (l) (r))]
       [(/) (Div (l) (r))]
       [else (parse-error)])]
    [else (parse-error)])))

;; parse an s-expression to a post-item
(define (parse-post-item x)
  (cond
    [(s-exp-symbol? x)
     (case (s-exp->symbol x)
       [(+) (AddOp)]
       [(-) (SubOp)]
       [(*) (MulOp)]
       [(/) (DivOp)]
       [else (error 'parse-post-item "bad postfix operator")])]
     [else (Expr (parse-sx x))]))

;; evaluates a postfix sequence of items, using a stack
(define (post-eval [items : (Listof PostfixItem)] stack)
  (cond
    ;; If no operations, return the unique value on the stack.
    [(and (empty? items) (empty? stack))
     (error 'post-eval "no value on the stack to return")]
    [(and (empty? items) (empty? (rest stack)))
     (first stack)]
    [(empty? items)
     (error 'post-eval "too many values left on the stack")]

    ;; Otherwise, there's an operation to perform
    [else
     (local [(define (pop2-and-apply op)
               (cond
                 [(or (empty? stack) (empty? (rest stack)))
                  (error 'post-eval "insufficient stack values")]
                 [else
                  (post-eval (rest items) (cons (op (second stack)
                                                    (first stack))
                                                (rest (rest stack))))]))]
     (type-case PostfixItem (first items)
       [(AddOp) (pop2-and-apply +)]
       [(SubOp) (pop2-and-apply -)]
       [(MulOp) (pop2-and-apply *)]
       [(DivOp) (pop2-and-apply /)]
       [(Expr exp) (post-eval (rest items) (cons (eval exp) stack))]))]))
      
;; evaluates PUAE expressions by reducing them to numbers
(define (eval expr)
  (type-case PUAE expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Post items) (post-eval items '())]))

;; evaluate a PUAE program contained in an s-exp
(define (run sx)
  (eval (parse-sx sx)))

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
  
(define minutes-spent 100)