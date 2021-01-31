#lang racket

(require "change-app.rkt")

; applying (λ (a) a)
((λ (a) a) 1)

(define (id n) n)
; applying id
(id 2)

(begin-for-syntax
  (define (id-in-syntax n) n)
  ; won't be changed
  (id-in-syntax 2))
