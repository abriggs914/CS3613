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
;; ...
  [json (key : String) (val : JPair)]
  [jstring (val : String)]
  [jarray (val : (Listof Number))]
  [jboolean (val : JSON)]
  [true (val : Boolean)]
  [false (val : Boolean)]
  [jnumber (val : Number)]
  ;[jdict (val : Dict)]
)

#|
; Question 3


;Question 4
|#

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


; Question 2
(test (jstring? (jstring "hello"))  #t)
(test (jarray? (jarray (list (jboolean true)
                           (jnumber 10))))  #t)
;(test (jdict? (jdict (list (jpair "happy"
;                                 (jboolean true))
;                          (jpair "cookies"
;                                 (jnumber 10)))))  #t)
#|
;(test (jpair->string (jpair "Alice" (jstring "Cryptologist")))
       "\"Alice\":\"Cryptologist\"")

(test (json->string
       (jarray (list (jnumber 5)
                     (jnumber 10))))  "[5,10]")
|#
#|
; Question 3
(test (json->string
       (jdict (list (jpair "happy"
                           (jboolean true))
                    (jpair "crazy"
                           (jboolean false)))))
                           "{\"happy\":true,\"crazy\":false}")

|#