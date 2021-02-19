#lang racket/base

(require (for-syntax racket/base
                     "env.rkt")
         "add.rkt")

(provide my-fun)

(begin-for-syntax
  (env-add! #'my-fun "my-fun from mul.rkt")
  (printf "mul.rkt: your-fun is: ~s\n" (env-ref #'your-fun)))
(define (my-fun a b)
  (* a (your-fun b)))
