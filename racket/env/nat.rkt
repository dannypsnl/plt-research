#lang racket

(provide (for-syntax zero suc)
         zero suc)

(require (for-syntax "env.rkt"))

(begin-for-syntax
  (define Bool 'Bool)
  (define true 'true)
  (define false 'false)

  (define Nat 'Nat)
  (define zero 'zero)
  (define (suc n) `(suc ,n)))

(define Bool 'Bool)
(define true 'true)
(define false 'false)

(define Nat 'Nat)
(define zero 'zero)
(define (suc n) `(suc ,n))
