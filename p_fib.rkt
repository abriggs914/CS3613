#lang racket/base
(require racket/future)
(require future-visualizer)

(define (p-fib n)
  (if (<= n 1) n
      (let ([x (future (lambda () (p-fib (- n 1))))]
	    [y (p-fib (- n 2))])
	(+ (touch x) y))))

(visualize-futures
  (p-fib 10))