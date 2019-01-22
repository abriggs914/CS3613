#lang plai
(define-type JSON 
  [jstring (val string?)]
  [jboolean (val boolean?)]
  [jnumber (val number?)]
  [jarray (items (listof JSON?))]
  [jdict (pairs (listof jpair?))]
  [jpair (key jstring?) (val JSON?)])


(test (JSON? (jstring "hello")) #t)
(test (JSON?  "hello") #f)
(test (JSON? (jnumber 10)) #t)

(define dict-1 
  (jdict 
   (list 
    (jpair 
     (jstring "Alice")
     (jstring "Cryptologist")
     )
    )
   )
  )

(test (JSON? dict-1) #t)
(define dict-2 
  (jdict (list 
          (jpair [jstring "Alice"]
                 [jstring "Cryptologist"])
          (jpair [jstring "Bob"]
                 [jarray 
                  (list
                   (jstring "Criminal")
                   (jnumber 42))]))))

(test (JSON? dict-2) #t)
(define (upcase-keys jval)
  (type-case JSON jval
    [jpair (key val) 
           (jpair 
            (jstring (string-upcase 
                      (jstring-val key)))
                  (upcase-keys val))]
    [jdict (l) (jdict (map upcase-keys l))]
    [jarray (l) (jarray (map upcase-keys l))]
    [else jval]))

(test (equal? 
       (upcase-keys dict-1)
       (jdict (list 
               (jpair 
                (jstring "ALICE") 
                (jstring "Cryptologist")))))
      #t)
(define (json->string val)
  (type-case JSON val
    [jboolean (b)  (if b "true" "false")]
    [jnumber (n) (number->string n)]
    [jstring (txt) (string-append 
                    "\"" txt "\"")]
    [jpair (key val) (string-append 
                      (json->string key) ":" 
                      (json->string val))]
    [jarray (l) (string-append 
                 "[" (comma-join l) "]")]
    [jdict (l) (string-append 
                "{" (comma-join l)  "}")]))

(define (comma-join lst) 
  (string-join (map json->string lst) ","))
(test (equal? (json->string dict-1) 
              "{\"Alice\":\"Cryptologist\"}") #t)
  
(test (equal? (json->string dict-2) 
              "{\"Alice\":\"Cryptologist\",\"Bob\":[\"Criminal\",42]}")
      #t)