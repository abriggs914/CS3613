#lang plait

;Fill in a definition for the function sumtree so that it sums up all the numbers in a numtree.
(define-type numtree
  [leaf (n : Number)]
  [tree (l : numtree) (r : numtree)])

(define (sumtree t)
  (type-case numtree t
    [(leaf n) n]
    [(tree l r) (+ (sumtree l) (sumtree r))]))

(test (sumtree (tree
                (tree (leaf 3) (leaf 4))
                (tree (leaf 5) (tree (leaf 6) (leaf 7)))))
       25)

(define-type AE
  [Num  (val : Number)]
  [Add  (l : AE) (r : AE)]
  [Sub  (l : AE) (r : AE)]
  [Mul  (args : (Listof AE))]
  [Div  (l : AE) (r : AE)])

;(AE -> Number)
(define (eval expr)
  (type-case AE expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul args) (mul-helper args 1)]
    [(Div l r) (/ (eval l) (eval r))]))

(define (mul-helper args acc)
  (cond
    [(empty? args) acc]
    [else (mul-helper (rest args) (* acc (eval (first args))))]))

(test (eval (Mul (list (Add (Num 1) (Num 2)) (Num 3) (Num 4)))) 36)

;(('a -> 'b) (Listof 'a) -> (Listof 'b))
(define (map [fun : ('a -> 'b)] [lst : (Listof 'a) ]) : (Listof 'b)
    (local
      [(define (helper f lst acc)
         (cond
           [(empty? lst) acc]
           [else (helper f (rest lst) (cons (f (first lst)) acc))]))]
      (reverse (helper fun lst (list)))))
(test
 (map add1 (list 1 2 3 4 5))
 (list 2 3 4 5 6))

#|(define-type EXPR
  [pair (l : Number) (r : Number)]
  [expr (p : EXPR) (s : Symbol)])|#

(define-type EXPR1
  [pair (a : Number) (b : Number)])

(define-type EXPR2
  [expr (a : EXPR1) (b : String)])

(define (eval2 exp)
  (type-case EXPR2 exp
    [(expr expr1 op)
     (type-case EXPR1 expr1
       [(pair a b)
        (cond
          [(eq? "+" op) (+ a b)]
          [(eq? "*" op) (* a b)]
          [(eq? "/" op) (/ a b)]
          [(eq? "-" op) (- a b)])])]))

(define ab
  (pair 1 3))
(define ba
  (expr ab "+"))
;(test ((pair 1 3) +) 4)
;(test ((pair 1 3) -) -2)
;(test ((pair 2 3) *) 6)

(define (f) 8)
(has-type f : (-> 'b))
(define h  (lambda (x y)
         (string-length (string-append x y))))

(has-type h : ('a 'b -> 'c))

(define (glonk f g)
  (lambda (x)
    (let* ((h (g x))
           (i (f h)))
      i)))
(has-type glonk : ('a 'b -> 'c))