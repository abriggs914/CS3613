#lang racket
(provide let-transformer)
    (define (let-transformer lst)
      (match lst
        [(list 'Let* '() body)     body]
        [(list 'Let* (cons (list id val      ) tail) body)
         (list 'let  (list (list id val))
               (let-transformer
                (list 'Let*          )))]))