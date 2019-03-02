#lang plait

(define (alpha f g)
  (lambda (x) (g (f x))))

(define-type WAE
    [Num  (val : Number)]
    [Add  (l : WAE) (r : WAE)]
    ;[Sub  (l : WAE) (r : WAE)]
    ;[Mul  (l : WAE) (r : WAE)]
    ;[Div  (l : WAE) (r : WAE)]
    [Id   (name : Symbol)]
    [With (name : Symbol) (val : WAE) (expr : WAE)])

(define (find-ids expr)
  (type-case WAE expr
    [(With id exp1 exp2)
     (append (list id) (find-ids exp2))]
    [(Add left right) (append (find-ids left) (find-ids right))]
    [else empty]))

(test (find-ids (Num 3 )) '())
(test (find-ids (Id 'x)) '())
(test (find-ids (With 'x (Num 3) (Id 'x))) '(x))
(test (find-ids (Add
                  (With 'x (Num 3) (Id 'x))
                  (With 'y (Num 3) (Id 'y)))) '(x y))
(test (find-ids (Add
                  (With 'x (Num 3) (Id 'x))
                  (With 'x (Num 3) (Id 'x)))) '(x x))
