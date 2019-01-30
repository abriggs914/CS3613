#lang plait

;; Question 1

;; Helper is defined globally so it can be re-used in later questions
(define (lst->string lst)
  (foldr string-append "" lst))

(define (comma-join formatter lst) : String
  (cond
    [(empty? lst) ""]
    [(empty? (rest lst)) (formatter (first lst))]
    [else (lst->string (list (formatter (first lst))
                             ","
                             (comma-join formatter (rest lst))))]))

(module+ test
  ;; For coverage of comma-join branches
  (test (comma-join to-string empty)  "")
  (test (comma-join to-string (list 1))  "1")
  (test (comma-join to-string (list 1 2 3))  "1,2,3")

  ;; Test  different formatter functions
  (test (comma-join symbol->string '(a b c)) "a,b,c")
  (test (comma-join identity '("a" "b" "c")) "a,b,c")
  (test (comma-join (lambda (p) (to-string (fst p))) (list (values 1 2) (values 3 4))) "1,3")

  ;; Test a larger input

  (test (comma-join to-string (build-list 10 identity)) "0,1,2,3,4,5,6,7,8,9"))

;; Question 2

(define-type JPair
  [jpair (key : String) (val : JSON)])

(define-type JSON
  [jstring (val : String) ]
  [jboolean (val : Boolean)]
  [jnumber (val : Number)]
  [jarray (val : (Listof JSON))]
  [jdict (val : (Listof JPair))])

(module+ test
  ;; Test variant constructors and predicates
  (test (jstring? (jstring "hello"))  #t)
  (test (jboolean? (jboolean #f))  #t)
  (test (jnumber? (jnumber 1))  #t)
  (test (jarray? (jarray (list (jboolean #t)
                               (jnumber 10))))  #t)
  (test (jdict? (jdict (list (jpair "happy"
                                    (jboolean #t))
                             (jpair "cookies"
                                    (jnumber 10)))))  #t)


  (define pair-1 (jpair "Alice"  (jstring "Cryptologist")))

  (define dict-1
    (jdict
     (list pair-1)))

  (test (jdict? dict-1)  #t)

  (define dict-2
    (jdict (list
            pair-1
            (jpair "Bob"
                   [jarray
                    (list
                     (jboolean #t)
                     (jboolean #f)
                     (jstring "Criminal")
                     (jnumber 42))]))))

  (test (jdict? dict-2)  #t))

;; Question 3

(define (jpair->string pair)
  (type-case JPair pair
    [(jpair key val) (lst->string (list "\"" key "\"" ":" (json->string val)))]))

(define (json->string val)
  (type-case JSON val
    [(jboolean b)  (if b "true" "false")]
    [(jnumber n) (to-string n)]
    [(jstring txt) (lst->string (list "\"" txt "\""))]
    [(jarray l) (lst->string
                 (list "[" (comma-join json->string l ) "]"))]
    [(jdict l) (lst->string
                (list "{" (comma-join jpair->string l)  "}"))]))

(module+ test
  ;; Coverage for jpair->string
  (test (jpair->string pair-1) "\"Alice\":\"Cryptologist\"")

  ;; Check different "simple" key-types
  (test (jpair->string (jpair "Cryptologist" (jboolean #t))) "\"Cryptologist\":true")
  (test (jpair->string (jpair "Midterms" (jnumber 1))) "\"Midterms\":1")

  ;; Complete coverage for json->string
  (test (json->string
         (jarray (list (jnumber 5)
                       (jnumber 10))))  "[5,10]")
  (test (json->string
         (jdict (list (jpair "happy"
                             (jboolean #t))
                      (jpair "crazy"
                             (jboolean #f)))))
        "{\"happy\":true,\"crazy\":false}")

  ;; test a second dictionary
  (test (json->string dict-2)
        "{\"Alice\":\"Cryptologist\",\"Bob\":[true,false,\"Criminal\",42]}")

  ;; Corner case: empty lists

  (test (json->string (jarray '())) "[]")
  (test (json->string (jdict '())) "{}")

  ;; Corner case: empty string

  (test (json->string (jstring "")) "\"\""))

(define minutes-spent 120)