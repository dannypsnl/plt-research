#lang racket

(provide (for-syntax Nat zero suc
                     Bool true false
                     Vec)
         Nat zero suc
         Bool true false
         Vec)

(begin-for-syntax
  (define Bool 'Bool)
  (define true 'true)
  (define false 'false)

  (define Nat 'Nat)
  (define zero 'zero)
  (define (suc n) `(suc ,n))

  (define (Vec T n) `(Vec ,T ,n))
  )

(define Bool 'Bool)
(define true 'true)
(define false 'false)

(define Nat 'Nat)
(define zero 'zero)
(define (suc n) `(suc ,n))

(define (Vec T n) `(Vec ,T ,n))
