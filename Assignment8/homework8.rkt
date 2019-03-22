#lang plait #:untyped

#|
 CS3613 Homework 8
 Mar.22/19
 Avery Briggs
 3471065
|#

(define-syntax-rule (lambda/rec* (f x ...) E)
  (let ([g
         (lambda/rec f
                     (lambda (bogus)
                       (let ([f
                              (lambda (x ...) ((f f) x ...))])
                         (lambda (x ...) E))))])
    (g #f)))

(define-syntax-rule (lambda/rec fun def)
  ((lambda (f)
     ((lambda (x) (f (lambda (n) ((x x) n))))
      (lambda (x) (f (lambda (n) ((x x) n))))))
   (lambda (fun) def)))

;; Testing ackermann
(define (acker-orig m n)
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (acker-orig (- m 1) 1)]
        [else      (acker-orig (- m 1)
                              (acker-orig m (- n 1)))]))
(test (acker-orig 0 0) 1)
(test (acker-orig 3 3) 61)
(test (acker-orig 3 4) 125)
(test (acker-orig 4 0) 13)

(let ([ackermann
       (lambda/rec ackermann
                   (lambda (m) 
                     (lambda (n)
                       (cond [(zero? m) (+ n 1)]
                             [(zero? n) ((ackermann (- m 1)) 1)]
                             [else ((ackermann (- m 1))
                                               ((ackermann m) (- n 1)))]))))])
  (begin
    (test ((ackermann 0) 0) 1)
    (test ((ackermann 3) 3) 61)
    (test ((ackermann 3) 4) 125)
    (test ((ackermann 4) 0) 13)))

(let* ([make-fact
         (lambda (self)
             (let ([ fact
                     (lambda (n) (( self self) n))])
               (lambda (n)
                 (if (zero? n)
                     1
                     (* n (fact (- n
                                   1)))))))]
       [fact ( make-fact make-fact )])
  (begin
   (test (fact 5) 120)
   (test (fact 8) 40320)
   (test (fact 12) 479001600)
   (test (fact 0) 1)))

(let* ([ackermann-2
        (lambda (self) 
          (let ([ackermann-2
                 (lambda (m n) ((self self) m n))])
            (lambda (m n)
              (cond [(zero? m) (+ n 1)]
                    [(zero? n) (ackermann-2 (- m 1) 1)]
                    [else (ackermann-2 (- m 1)
                                      (ackermann-2 m (- n 1)))]))))]
       [ackermann2 (ackermann-2 ackermann-2)])
  (begin
    (test (ackermann2 0 0) 1)
    (test (ackermann2 3 3) 61)
    (test (ackermann2 3 4) 125)
    (test (ackermann2 4 0) 13)))

;; let* implementation
(let*
    ([fact
      (lambda/rec
       fact
         (lambda (n)
           (if (zero? n)
               1
               (* n (fact (- n 1))))))]
     [fact0 (lambda (y) (fact y))])
  (begin
   (test (fact0 5) 120)
   (test (fact0 8) 40320)
   (test (fact0 12) 479001600)
   (test (fact0 0) 1)))

;; let* implementation
(let* ([ackermann
        (lambda/rec
         ackermann
         (lambda (self)
           (let ([ackermann (lambda (x y) ((self self) x y))])
             (lambda (m n)
               (cond [(zero? m) (+ n 1)]
                     [(zero? n) (ackermann (- m 1) 1)]
                     [else      
                      (ackermann (- m 1)
                                 (ackermann m (- n 1)))])))))]
       [ackermann2 (ackermann ackermann)])
  (begin
    (test (ackermann2 0 0) 1)
    (test (ackermann2 3 3) 61)
    (test (ackermann2 3 4) 125)
    (test (ackermann2 4 0) 13)))

(let ([ackermann
       (lambda/rec*  (ackermann m n)
                       (cond [(zero? m) (+ n 1)]
                             [(zero? n) (ackermann (- m 1) 1)]
                             [else      (ackermann (- m 1)
                                                   (ackermann m (- n 1)))]))])
  (begin
    (test (ackermann 0 0) 1)
    (test (ackermann 3 3) 61)
    (test (ackermann 3 4) 125)
    (test (ackermann 4 0) 13)))

(let ([fact
       (lambda/rec* (fact n)
                      (cond
                        [(= n 0) 1]
                        [else (* n (fact (- n 1)))]))])
  (begin
   (test (fact 5) 120)
   (test (fact 8) 40320)
   (test (fact 12) 479001600)
   (test (fact 0) 1)))

(let ([fib
        (lambda/rec* (fib n)
                     (if (<= n 1) n
                         (+ (fib (- n 1))
                            (fib (- n 2)))))])
  (begin
    (test (fib 8) 21)
    (test (fib 28) 317811)
    (test (fib 1) 1)
    (test (fib 0) 0)))
