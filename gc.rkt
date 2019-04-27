#lang plait

(define-type FAE
  [Num (n : Number)]
  [Add (lhs : FAE)
       (rhs : FAE)]
  [Sub (lhs : FAE)
       (rhs : FAE)]
  [Id (name : Symbol)]
  [Fun (param : Symbol)
       (body : FAE)]
  [Call (fun-expr : FAE)
       (arg-expr : FAE)]
  [If0 (test : FAE)
       (then : FAE)
       (else : FAE)])

#|
(define-type CFAE
8  [cnum (n : Number)]
9  [cadd (lhs CFAE?)
        (rhs CFAE?)]
10  [csub (lhs CFAE?)
        (rhs CFAE?)]
11  [cid (pos : Number)]
12  [cfun (body CFAE?)]
13  [ccall (fun-expr CFAE?)
       (arg-expr CFAE?)]
14  [cif0 (test CFAE?)
        (then CFAE?)
        (else CFAE?)])
|#

#|
(define-type CFAE-Value
15  [numV (n : Number)]
16  [closureV (body CFAE?)
            (ds DefrdSub?)])
|#

(define-type CDefrdSub
  [mtCSub]
  [aCSub (name : Symbol)
         (rest : CDefrdSub)])

;; A DefrdSub is a list of CFAE-Value
; (define (DefrdSub? x) true)

;; ----------------------------------------
;; Allocator for code, which is never freed;
;; use code-ref instead of ref to refer to code

(define code-memory (make-vector 2048 0))
(define code-ptr 0)

(define (code-incptr n)
  (begin
    (set! code-ptr (+ code-ptr n))
    (- code-ptr n)))

;; code-malloc1 : number number -> number
(define (code-malloc1 tag a)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (code-incptr 2)))

;; code-malloc2 : number number number -> number
(define (code-malloc2 tag a b)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (vector-set! code-memory (+ code-ptr 2) b)
    (code-incptr 3)))

;; code-malloc3 : number number number number -> number
(define (code-malloc3 tag a b c)
  (begin
    (vector-set! code-memory code-ptr tag)
    (vector-set! code-memory (+ code-ptr 1) a)
    (vector-set! code-memory (+ code-ptr 2) b)
    (vector-set! code-memory (+ code-ptr 3) c)
    (code-incptr 4)))

;; code-ref : number number -> number
(define (code-ref n d)
  (vector-ref code-memory (+ n d)))
           
;; ----------------------------------------

;; parse : S-expr -> FAE
(define (parse-error sx)
  (error 'parse (string-append "parse error: " (to-string sx))))

(define (sx-ref sx n) (list-ref (s-exp->list sx) n))

(define (parse sx)
  (local
      [(define (px i) (parse (sx-ref sx i)))]
    (cond
      [(s-exp-number? sx) (Num (s-exp->number sx))]
      [(s-exp-symbol? sx) (Id (s-exp->symbol sx))]
      [(s-exp-match? `(fun (SYMBOL) ANY) sx)
       (let* ([args (sx-ref sx 1)]
              [id (s-exp->symbol (sx-ref args 0))]
              [body (px 2)])
         (Fun id body))] 
      [(s-exp-match? `(ANY ANY) sx) (Call (px 0) (px 1))]
      [(s-exp-list? sx)
       (case (s-exp->symbol (sx-ref sx 0))
         [(+) (Add (px 1) (px 2))]
         [(-) (Sub (px 1) (px 2)) ]
         [(if0) (If0 (px 1) (px 2) (px 3))]
         [else (parse-error sx)])]
      [else (parse-error sx)])))

(module+ test
  (print-only-errors #t)
  (test (parse `3) (Num 3))
  (test (parse `x) (Id 'x))
  (test (parse `{+ 1 2}) (Add (Num 1) (Num 2)))
  (test (parse `{- 1 2}) (Sub (Num 1) (Num 2)))
  (test (parse `{fun {x} x}) (Fun 'x (Id 'x)))
  (test (parse `{f 2}) (Call (Id 'f) (Num 2)))
  (test (parse `{if0 0 1 2}) (If0 (Num 0) (Num 1) (Num 2))))

;; ----------------------------------------

;; compile : FAE CDefrdSub -> CFae
(define (compile a-fae ds)
  (type-case FAE a-fae
    [(Num n) (code-malloc1 8 n)]
    [(Add l r) (code-malloc2 9 (compile l ds) (compile r ds))]
    [(Sub l r) (code-malloc2 10 (compile l ds) (compile r ds))]
    [(Id name) (code-malloc1 11 (locate name ds))]
    [(Fun param body-expr) 
         (code-malloc1 12 (compile body-expr (aCSub param ds)))]
    [(Call fun-expr arg-expr)
         (code-malloc2 13 
                       (compile fun-expr ds)
                       (compile arg-expr ds))]
    [(If0 test-expr then-expr else-expr)
         (code-malloc3 14
                       (compile test-expr ds)
                       (compile then-expr ds)
                       (compile else-expr ds))]))

(define (locate name ds)
  (type-case CDefrdSub ds
    [(mtCSub) (error 'locate "free variable")]
    [(aCSub sub-name rest-ds)
           (if (equal? sub-name name)
               0
               (+ 1 (locate name rest-ds)))]))

;; ----------------------------------------
;; Memory allocator with a 2-space collector
;;  for run-time allocation

(define MEMORY-SIZE 128)

(define space1 (make-vector MEMORY-SIZE 0))
(define space2 (make-vector MEMORY-SIZE 0))

(define memory space1)
(define ptr 0)

(define from-memory (make-vector 0 0))

(define result-reg 0)

(define (incptr n)
  ;; Increment the allocation pointer, and
  ;;  if there's not enough room for the next
  ;;  allocation, then collect garbage
  (begin
    (set! ptr (+ ptr n))
    (if (>= (+ ptr 5) MEMORY-SIZE)
        (begin
          (set! result-reg (- ptr n))
          (if (> (vector-length from-memory) 0)
              ;; Ran out of space while GCing
              ;;  => GCing didn't reclaim anything,
              ;;     so we're really out of space
              (error 'malloc "out of memory")
              (gc)))
        (- ptr n))))

;; malloc1 : number number -> number
(define (malloc1 tag a)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (incptr 2)))

;; malloc2 : number number number -> number
(define (malloc2 tag a b)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (incptr 3)))

;; malloc3 : number number number number -> number
(define (malloc3 tag a b c)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (vector-set! memory (+ ptr 3) c)
    (incptr 4)))

;; malloc4 : number number number number number -> number
(define (malloc4 tag a b c d)
  (begin
    (vector-set! memory ptr tag)
    (vector-set! memory (+ ptr 1) a)
    (vector-set! memory (+ ptr 2) b)
    (vector-set! memory (+ ptr 3) c)
    (vector-set! memory (+ ptr 4) d)
    (incptr 5)))

;; ref : number number -> number
(define (ref n d)
  (vector-ref memory (+ n d)))

;; Pointer in to space; objects before the
;;  pointer are "black", and object after are "gray"
(define updated-ptr 0)

(define (gc)
  (begin
    (display "GCing\n")
    ;; Swap to and from space:
    (set! from-memory memory)
    (if (eq? memory space1)
        (set! memory space2)
        (set! memory space1))
    (set! ptr 0)
    ;; Update registers to start:
    (set! v-reg (move v-reg))
    (set! ds-reg (move ds-reg))
    (set! k-reg (move k-reg))
    (set! result-reg (move result-reg))
    (set! updated-ptr 0)
    ;; Loop until there are no gray objects:
    (update)))


(define (update)
  (if (= updated-ptr ptr)
      ;; No more gray objects:
      (begin
        (set! from-memory (make-vector 0 0))
        result-reg)
      ;; updated-ptr points to first gray object:
      (case (ref updated-ptr 0)
        [(0 15)
         ;; Record has just an integer
         (done 1)]
        [(1 3 5) 
         (begin
           ;; Record has two run-time pointers
           ;;  in slots 2 and 3 (and an integer in 1)
           (move! 2)
           (move! 3)
           (done 3))]
        [(2 4 6 17) 
         (begin
           ;; Etc.
           (move! 1)
           (move! 2)
           (done 2))]
        [(16)
         (begin
           (move! 2)
           (done 2))]
        [(7)
         (begin
           (move! 3)
           (move! 4)
           (done 4))])))

(define (done n)
  (begin
    (set! updated-ptr (+ updated-ptr (+ n 1)))
    (update)))


;; move! : number -> void
;;  Updates pointer at updated-ptr+n, moving the
;;  target as necessary:
(define (move! n)
  (vector-set! memory (+ updated-ptr n)
               (move (vector-ref memory (+ updated-ptr n)))))

;; move : number -> number
;;  If n refers to a white record, copy it to to-space and
;;   insert a forwarding pointer, so now it's gray
;; If n refers to a gray/black record, return the forwarding
;;   pointer.
(define (move n)
  (if (= 18 (vector-ref from-memory n))
      ;; Gray/black:
      (vector-ref from-memory (+ n 1))
      ;; White:
      (begin
        (case (vector-ref from-memory n)
          [(0 15)
           ;; size 1
           (begin
             (malloc1 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1)))
             (vector-set! from-memory (+ n 1) (- ptr 2)))]
          [(2 4 6 16 17)
           ;; size 2
           (begin
             (malloc2 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2)))
             (vector-set! from-memory (+ n 1) (- ptr 3)))]
          [(1 3 5)
           ;; size 3
           (begin
             (malloc3 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2))
                      (vector-ref from-memory (+ n 3)))
             (vector-set! from-memory (+ n 1) (- ptr 4)))]
          [(7)
           ;; size 4
           (begin
             (malloc4 (vector-ref from-memory n)
                      (vector-ref from-memory (+ n 1))
                      (vector-ref from-memory (+ n 2))
                      (vector-ref from-memory (+ n 3))
                      (vector-ref from-memory (+ n 4)))
             (vector-set! from-memory (+ n 1) (- ptr 5)))])
        ;; Change to gray:
        (vector-set! from-memory n 18)
        ;; Return forwarding porter (that we just installed):
        (vector-ref from-memory (+ n 1)))))

;; ----------------------------------------

(define fae-reg 0)
(define ds-reg 0)

;; interp : FAE DefrdSub FAE-Cont -> alpha
(define (interp)
  (case (code-ref fae-reg 0)
    [(8) ; num
     (begin
       (set! v-reg (malloc1 15 (code-ref fae-reg 1)))
       (continue))]
    [(9) ; add
     (begin
       (set! k-reg (malloc3 1
                            (code-ref fae-reg 2)
                            ds-reg 
                            k-reg))
       (set! fae-reg (code-ref fae-reg 1))
       (interp))]
    [(10) ; sub
     (begin
       (set! k-reg (malloc3 3
                            (code-ref fae-reg 2)
                            ds-reg k-reg))
       (set! fae-reg (code-ref fae-reg 1))
       (interp))]
    [(11) ; id
     (begin
       (set! ds2-reg ds-reg)
       (set! v-reg (code-ref fae-reg 1))
       (sc-ref))]
    [(12) ; fun
     (begin
       (set! v-reg (malloc2 16 (code-ref fae-reg 1) ds-reg))
       (continue))]
    [(13) ; call
     (begin
       (set! k-reg (malloc3 5
                            (code-ref fae-reg 2)
                            ds-reg k-reg))
       (set! fae-reg (code-ref fae-reg 1))
       (interp))]
    [(14) ; if0
     (begin
       (set! k-reg (malloc4 7
                            (code-ref fae-reg 2)
                            (code-ref fae-reg 3)
                            ds-reg k-reg))
       (set! fae-reg (code-ref fae-reg 1))
       (interp))]))

(define k-reg 0)
(define v-reg 0)

#|
(define-type CFAE-Cont
0  [mtK]
1  [addSecondK (r CFAE?)
              (ds DefrdSub?)
              (k CFAE-Cont?)]
2  [doAddK (v1 CFAE-Value?)
          (k CFAE-Cont?)]
3  [subSecondK (r CFAE?)
              (ds DefrdSub?)
              (k CFAE-Cont?)]
4  [doSubK (v1 CFAE-Value?)
          (k CFAE-Cont?)]
5  [callArgK (arg-expr CFAE?)
           (ds DefrdSub?)
           (k CFAE-Cont?)]
6  [doCallK (fun-val CFAE-Value?)
          (k CFAE-Cont?)]
7  [doIfK (then-expr CFAE?)
         (else-expr CFAE?)
         (ds DefrdSub?)
         (k CFAE-Cont?)])
|#

#|
 17 cons for subcache
 18 moved
|#

;; continue : -> void
(define (continue)
  (case (ref k-reg 0)
    [(0) ; mtk
     v-reg]
    [(1) ; addSecondK
     (begin
       (set! fae-reg (ref k-reg 1))
       (set! ds-reg (ref k-reg 2))
       (set! k-reg (malloc2 2 v-reg (ref k-reg 3)))
       (interp))]
    [(2) ; doAddK
     (begin
       (set! v-reg (num+ (ref k-reg 1) v-reg))
       (set! k-reg (ref k-reg 2))
       (continue))]
    [(3) ; subSecondK
     (begin
       (set! fae-reg (ref k-reg 1))
       (set! ds-reg (ref k-reg 2))
       (set! k-reg (malloc2 4 v-reg (ref k-reg 3)))
       (interp))]
    [(4) ; doSubK
     (begin
       (set! v-reg (num- (ref k-reg 1) v-reg))
       (set! k-reg (ref k-reg 2))
       (continue))]
    [(5) ; callArgK
     (begin
       (set! fae-reg (ref k-reg 1))
       (set! ds-reg (ref k-reg 2))
       (set! k-reg (malloc2 6 v-reg (ref k-reg 3)))
       (interp))]
    [(6) ; doCallK
     (begin
       (set! fae-reg (ref (ref k-reg 1) 1))
       (set! ds-reg (malloc2 17
                             v-reg
                             (ref (ref k-reg 1) 2)))
       (set! k-reg (ref k-reg 2))
       (interp))]
    [(7) ; doIfK
     (begin
       (if (numzero? v-reg)
           (set! fae-reg (ref k-reg 1))
           (set! fae-reg (ref k-reg 2)))
       (set! ds-reg (ref k-reg 3))
       (set! k-reg (ref k-reg 4))
       (interp))]))

;; num-op : (number number -> number) -> (FAE-Value FAE-Value -> FAE-Value)
(define (num-op op op-name)
  (lambda (x y)
    (malloc1 15 (op (ref x 1) (ref y 1)))))

(define num+ (num-op + '+))
(define num- (num-op - '-))

;; numzero? : FAE-Value -> boolean
(define (numzero? x)
  (zero? (ref x 1)))

(define ds2-reg 0)

(define (sc-ref)
  (if (zero? v-reg)
      (begin
        (set! v-reg (ref ds2-reg 1))
        (continue))
      (begin
        (set! ds2-reg (ref ds2-reg 2))
        (set! v-reg (- v-reg 1))
        (sc-ref))))

;; ----------------------------------------

(define (init-k) (malloc1 0 0))
(define (interpx a-fae ds k)
  (begin
    (set! fae-reg a-fae)
    (set! ds-reg ds)
    (set! k-reg k)
    (interp)))
(define (numV x) (malloc1 15 x))
(define (empty-ds) (malloc1 0 0))

(define (vtest a b)
  (test (ref a 1) (ref b 1)))

(define (reset!)
  (begin
    (set! code-ptr 0)
    (set! ptr 0)
    (set! v-reg 0)
    (set! fae-reg 0)
    (set! k-reg 0)
    (set! ds-reg 0)
    (set! result-reg 0)
    (void)))

(vtest (interpx (compile (Num 10) (mtCSub))
                (empty-ds)
                (init-k))
       (numV 10))
(reset!)
(vtest (interpx (compile (Add (Num 10) (Num 7)) (mtCSub))
                (empty-ds)
                (init-k))
       (numV 17))
(reset!)
(vtest (interpx (compile (Sub (Num 10) (Num 7)) (mtCSub))
                (empty-ds)
                (init-k))
       (numV 3))
(reset!)
(vtest (interpx (compile
                 (Call (Fun 'x (Add (Id 'x) (Num 12)))
                      (Add (Num 1) (Num 17)))
                 (mtCSub))
                (empty-ds)
                (init-k))
       (numV 30))
(reset!)
(vtest (interpx (compile (Id 'x)
                         (aCSub 'x (mtCSub)))
                (malloc2 17 (numV 10) (empty-ds))
                (init-k))
       (numV 10))
(reset!)
(vtest (interpx (compile (Call (Fun 'x (Add (Id 'x) (Num 12)))
                              (Add (Num 1) (Num 17)))
                         (mtCSub))
                (empty-ds)
                (init-k))
       (numV 30))
(reset!)
(vtest (interpx (compile (Call (Fun 'x
                                   (Call (Fun 'f
                                             (Add (Call (Id 'f) (Num 1))
                                                  (Call (Fun 'x
                                                            (Call (Id 'f) (Num 2)))
                                                       (Num 3))))
                                        (Fun 'y (Add (Id 'x) (Id 'y)))))
                              (Num 0))
                         (mtCSub))
                (empty-ds)
                (init-k))
       (numV 3))
(reset!)
(vtest (interpx (compile (If0 (Num 0)
                              (Num 1)
                              (Num 2))
                         (mtCSub))
                (empty-ds)
                (init-k))
       (numV 1))
(reset!)
(vtest (interpx (compile (If0 (Num 1)
                              (Num 0)
                              (Num 2))
                         (mtCSub))
                (empty-ds)
                (init-k))
       (numV 2))
(reset!)
(vtest (interpx (compile
                 (parse 
                  `{{fun {mkrec}
                         {{fun {fib}
                               ;; Call fib on 4:
                               {fib 4}}
                          ;; Create recursive fib:
                          {mkrec
                           {fun {fib}
                                ;; Fib:
                                {fun {n}
                                     {if0 n
                                          1
                                          {if0 {- n 1}
                                               1
                                               {+ {fib {- n 1}}
                                                  {fib {- n 2}}}}}}}}}}
                    ;; mkrec:
                    {fun {body-proc}
                         {{fun {fX}
                               {fX fX}}
                          {fun {fX}
                               {body-proc {fun {x} {{fX fX} x}}}}}}})
                 (mtCSub))
                (empty-ds)
                (init-k))
       (numV 5))

(test/exn (compile (Id 'x) (mtCSub))
          "free variable")