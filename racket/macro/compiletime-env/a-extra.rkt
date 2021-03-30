#lang racket/base

(require (for-syntax racket/base))
(begin-for-syntax
  (struct extrainfo (target data)
    #:property prop:rename-transformer (struct-field-index target))
  (provide extrainfo-data))

(provide (rename-out [my-extra-surface my-extra]))
(define-syntax my-extra-surface
  (extrainfo (syntax-property #'my-extra 'not-free-identifier=? #t)
             "mul.rkt: my-extra info"))
(define (my-extra a b)
  (+ a b))

