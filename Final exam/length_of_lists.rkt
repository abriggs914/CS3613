#lang plait
(define (yo dawg herd)
  (lambda (u)
    (herd (u dawg))))
#|
(test
 (let
     ((len
       (lambda (self list)
         (if (empty? list)
             0
             (add1
              (lambda (x) (self list))))))
   (len len  '(a b c d e)))) 5)
|#
#|
(test
 (let
     ((len
       (lambda (self list)
         (if (empty? list)
             0
             ((lambda (x) (self x)) list)))))
   (len len  '(a b c d e))) 5)
|#



(test
 (let
     ((len
       (lambda (self list)
         (if (empty? list)
             0
             (begin
               (display self)
               (+ (self (self (first list))) 0))))))
   (len len  '(a b c d e))) 5)