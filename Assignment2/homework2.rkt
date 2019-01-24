#lang plait

#|
  CS3613 Assignment2
  Jan.25/19
  Avery Briggs
  3471065
|#


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

(define (json->string val)
  (type-case JSON val
    [(json key jpair) (string-append (string-append 
                                      (to-string key) ":") 
                                     (jpair->string jpair))]
    [(jstring txt) (string-append (string-append 
                                   "\"" txt) "\"")]
    [(jarray l) (string-append (string-append 
                                "[" (comma-join json->string l)) "]")]
    [(jboolean bool) (if bool "true" "false")]
    [(jnumber num) (to-string num)]
    [(jdict dict) (string-append (string-append 
                                  "{" (comma-join jpair->string dict)) "}")]))

(define (jpair->string pair)
  (type-case JPair pair
    [(jpair key val) (string-append (string-append 
                                     (to-string key) ":") 
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
; lambda test string-append
(test (comma-join (λ (x) (string-append "Hello " x)) (list "Bob" "Sue")) "Hello Bob,Hello Sue")


; Question 2

(test (json? (json "thing" (jpair "pair" (jnumber 1000))))  #t)

(test (jstring? (jstring "hello"))  #t)

(test (jarray? (jarray (list (jboolean #t)
                             (jnumber 10))))  #t)

(test (jboolean? (jboolean #t)) #t)

(test (jnumber? (jboolean #f)) #f)

(test (jdict? (jdict (list (jpair "happy"
                                  (jboolean #t))
                           (jpair "cookies"
                                  (jnumber 10)))))  #t)

; Question 3

(test (jpair->string (jpair "Alice" (jstring "Cryptologist")))
      "\"Alice\":\"Cryptologist\"")

(test (json->string
       (jarray (list (jnumber 5)
                     (jnumber 10))))
      "[5,10]")

(test (json->string
       (jarray (list))) "[]")

(test (json->string
       (jdict (list (jpair "happy"
                           (jboolean #t))
                    (jpair "crazy"
                           (jboolean #f)))))
      "{\"happy\":true,\"crazy\":false}")

(test (json->string
       (jdict (list (jpair "happy"
                           (jboolean #t))
                    (jpair "crazy"
                           (jboolean #f))
                    (jpair "hungry"
                           (jboolean #t)))))
      "{\"happy\":true,\"crazy\":false,\"hungry\":true}")

(test (json->string (json "thing" (jpair "pair" (jnumber 1000))))
      "\"thing\":\"pair\":1000")

(test (json->string (jdict (list (jpair "" (jarray (list))))))
      "{\"\":[]}")