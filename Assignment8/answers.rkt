#lang plait #:untyped

;; 0 copy lambda/rec definition

  (define-syntax-rule (lambda/rec fun def)
    ((lambda (f)
       ((lambda (x) (f (lambda (n) ((x x) n))))
        (lambda (x) (f (lambda (n) ((x x) n))))))
     (lambda (fun) def)))


;;

(define (acker-orig m n)
  (cond [(zero? m) (+ n 1)]
	[(zero? n) (acker-orig (- m 1) 1)]
	[else      (acker-orig (- m 1)
			      (acker-orig m (- n 1)))]))
(test (acker-orig 3 3) 61)

#;(module+ fail

  (let ([ackermann
         (lambda/rec ackermann
             (lambda (m n)
               (cond [(zero? m) (+ n 1)]
                     [(zero? n) (ackermann (- m 1) 1)]
                     [else      (ackermann (- m 1)
                                           (ackermann m (- n 1)))])))])
     (ackermann 3 3)))

;; 1. Make curried version of acker-core

(let ([ackermann
       (lambda/rec ackermann
           (lambda (m)
             (lambda (n)
               (let ([ackermann (lambda (m n) ((ackermann m) n))])
                 (cond [(zero? m) (+ n 1)]
                       [(zero? n) (ackermann (- m 1) 1)]
                       [else      (ackermann (- m 1)
                                             (ackermann m (- n 1)))])))))])
  (test ((ackermann 3) 3) 61))




;; 3. Provide a wrapper for ackermann0 to allow uncurried use.

(let* ([ackermann
       (lambda/rec ackermann
           (lambda (m)
             (lambda (n)
               (let ([ackermann (lambda (m n) ((ackermann m) n))])
                 (cond [(zero? m) (+ n 1)]
                       [(zero? n) (ackermann (- m 1) 1)]
                       [else      (ackermann (- m 1)
                                             (ackermann m (- n 1)))])))))]
      [ackermann2 (lambda (x y) ((ackermann x) y))])

  (test (ackermann2 3 3) 61))



;; 4.

(let* ([fact (lambda/rec fact
                 (lambda (dummy)
                   (lambda (n)
                     (let ([fact (fact #f)])
                       (if (zero? n)
                           1
                           (* n (fact (- n 1))))))))]
       [fact0 (fact #f)])

  (test (fact0 5) 120))

;; work up to this
(let* ([ackermann
        (lambda/rec ackermann
            (lambda (dummy)
              (lambda (m n)
                (let ([ackermann (ackermann #f)])
                  (cond [(zero? m) (+ n 1)]
                        [(zero? n) (ackermann (- m 1) 1)]
                        [else      (ackermann (- m 1)
                                              (ackermann m (- n 1)))])))))]
       [ackermann2  (ackermann #f)])
  (test (ackermann2 3 3) 61))


(define-syntax-rule (lambda/rec* (f x ...) E)
  (let ([g  (lambda/rec f
                (lambda (_)
                  (lambda (x ...)
                    (let ([f (f #f)])
                      E))))])
             (g #f)))


(define-syntax-rule (define/fun (id x ...) body)
  (define id
    (lambda (x ...) body)))

#;(define/fun (ident x y z) y)

(let ([fact (lambda/rec* (fact n)
                (if (zero? n)
                    1
                    (* n (fact (- n 1)))))])
  (test (fact 5) 120))


(let ([ackermann (lambda/rec*  (ackermann m n)
                     (cond [(zero? m) (+ n 1)]
                           [(zero? n) (ackermann (- m 1) 1)]
                           [else      (ackermann (- m 1)
                                                 (ackermann m (- n 1)))]))])
  (test (ackermann 3 3) 61))

(let ([mul
       (lambda/rec* (mul a b)
           (cond
             [(<= a 0) 0]
             [(= a 1) b]
             [else (+ (mul (sub1 a) b) b)]))])
  (begin
    (test (mul 6 7) 42)
    (test (mul 7 0) 0)
    (test (mul 0 7) 0)
    (test (mul 1 7) 7)))

(define minutes-spent 240)
