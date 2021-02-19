#lang racket/base

(require (for-syntax racket/base
                     "env.rkt"))

(provide my-fun
         your-fun)


(begin-for-syntax
  (env-add! #'my-fun "my-fun from add.rkt"))
(define (my-fun a b)
  (+ a b))

(begin-for-syntax
  (env-add! #'your-fun "your-fun from add.rkt"))
(define (your-fun c)
  (- c))

;; See https://docs.racket-lang.org/syntax/syntax-helpers.html#%28part._.Dictionaries_for_free-identifier~3d_%29 
;; for the caveat of macro-generated definitions
