#lang racket

(provide 
         zero suc)

(require (for-syntax "env.rkt"))

(begin-for-syntax
  (env-add! #'zero 'Nat))
(define zero 'zero)
(begin-for-syntax
  (env-add! #'suc '(-> Nat Nat)))
(define (suc n) `(suc ,n))
