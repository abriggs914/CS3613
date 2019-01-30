#lang plait

(define-type WAE
[Num (val : Number)]
[Add (l : WAE) (r : WAE)]
[Sub (l : WAE) (r : WAE)]
[Mul (l : WAE) (r : WAE)]
[Div (l : WAE) (r : WAE)]
[Id (name : Symbol)]
[With (name : Symbol)
(val : WAE)
(expr : WAE)])

#|
(test/exn (parse-sx `{* 1 2 3}) "parse error")
(test/exn (parse-sx `{foo 5 6}) "parse error")
(test/exn (parse-sx `{with x 5 {* x 8}})
"parse error")
(test/exn (parse-sx `{with {5 x} {* x 8}})
"parse error")
(test (parse-sx `{with {x 5} {* x 8}})
(With 'x (Num 5) (Mul (Id 'x) (Num 8))))
|#