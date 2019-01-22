#lang plait


;(define (list-length-helper lst len)
 ;   (if (empty? lst) len
  ;      (list-length-helper (rest lst) 
   ;                         (+ len 1))))

(define (list-length lst-outer)
  (local [
(define (list-length-helper lst len)
    (if (empty? lst) len
        (list-length-helper (rest lst) 
                            (+ len 1))))] (list-length-helper lst-outer 0)))

(define big-list (build-list 10000000 identity))
(define (time-it) (time (list-length big-list)))

(time-it)

#|
(define (list-length-helper list len)
    (if (empty? list) len
        (list-length-helper (rest list) 
                            (+ len 1))))

(define (list-length list)
  (list-length-helper list 0))

(define big-list (build-list 10000000 identity))
(define (time-it) (time (list-length big-list)))
|#


(define (reverse lst)
  (letrec ([rev (λ (lst acc)
                  (cond
                    [(empty? lst) acc]
                    [else (rev (rest lst)
                               (cons (first lst) acc))]))])
    (rev lst empty)))

(module+ test
  (test (reverse empty) empty)
  (test (reverse (list 1 2 3)) (list 3 2 1)))