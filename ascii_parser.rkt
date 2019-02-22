#lang ragg
(require ragg/support)
(require "ragg_ascii.rkt")
(define stx
  (parse (list (token 'INTEGER 6)
               (token 'INTEGER 2)
               (token 'STRING " ")
               (token 'INTEGER 3)
               (token 'STRING "X")
               ";")))
(syntax->datum stx)