#lang plait

; Question 1
;(('a -> String) (Listof 'a) -> String)
(define (comma-join func lst)
  (cond
    [(empty? lst) ""]
    [else
     (local
       [(define (helper acc func lst)
          (cond
            [(empty? lst) acc]
            [(= 1 (length lst)) (helper (string-append acc (func (first lst))) func (rest lst))]
            [else (helper (string-append (string-append acc (func (first lst))) ",") func (rest lst))]))]
       (helper "" func lst))]))


;Question 2
(define-type JPair
  [jpair (key : String) (val : JSON)])

(define-type JSON
  [json (key : String) (val : JPair)]
  [jstring (val : String)]
  [jarray (val : (Listof JSON))]
  [jboolean (val : Boolean)]
  [jnumber (val : Number)]
  [jdict (val : (Listof JPair))])


; Question 3

;(define (jpair->string pair)
;  (....))

(define (json->string val)
  (type-case JSON val
    [(json key jpair) (string-append (string-append 
                      (json->string key) ":") 
                      (jpair->string jpair))]
    [(jstring txt) (string-append (string-append 
                    "\"" txt) "\"")]
    [(jarray l) (string-append (string-append 
                 "[" (comma-join to-string l)) "]")]
    [(jboolean b)  (if b "true" "false")]
    [(jnumber n) (to-string n)]
    [(jdict l) (string-append (string-append 
                "{" (comma-join to-string l))  "}")]))

(define (jpair->string pair)
  (type-case JPair pair
    [(jpair key val) (string-append (string-append 
                      (json->string key) ":") 
                      (json->string val))]))


;Question 4
(define minutes-spent 30)


; Testing

; Question 1
; Numbers
(test (comma-join to-string (list 1 2 3)) "1,2,3")
; Empty list returns empty string
(test (comma-join to-string (list)) "")
; Strings return with escape characters 
(test (comma-join to-string (list "Hello" "World")) "\"Hello\",\"World\"")
; symbols to strings
(test (comma-join symbol->string (list 'Hello 'World)) "Hello,World")

;; write a lambda test string-append


; Question 2
(test (jstring? (jstring "hello"))  #t)

(test (jarray? (jarray (list (jboolean #t)
                           (jnumber 10))))  #t)

(test (jdict? (jdict (list (jpair "happy"
                                 (jboolean #t))
                          (jpair "cookies"
                                 (jnumber 10)))))  #t)

; Question 3

(test (jpair->string (jpair "Alice" (jstring "Cryptologist")))
       "\"Alice\":\"Cryptologist\"")
#|
(test (json->string
       (jarray (list (jnumber 5)
                     (jnumber 10))))  "[5,10]")

(test (json->string
       (jdict (list (jpair "happy"
                           (jboolean #t))
                    (jpair "crazy"
                           (jboolean #f)))))
                           "{\"happy\":true,\"crazy\":false}")

|#