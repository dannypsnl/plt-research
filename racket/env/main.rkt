#lang racket

(require (for-syntax "env.rkt")
         "nat.rkt")

(begin-for-syntax
  (displayln (env-ref #'suc)))
(suc zero)
zero
