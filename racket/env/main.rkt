#lang racket

(require (for-syntax "app.rkt")
         "nat.rkt")

(begin-for-syntax
  (displayln (suc zero)))
(suc zero)
zero
