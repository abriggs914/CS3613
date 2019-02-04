#lang plait
(define ht (hash (list (pair "apple"  'red) (pair "banana"  'yellow))))

(module+ test
  (test (hash-ref ht "apple") (some 'red)))

(define ht2 (hash-set ht "coconut" 'brown))

#|
(module+ test
  (test (hash-ref ht2 "coconut") 'brown)
  (test/exn (hash-ref ht "coconut")) "exception")
|#