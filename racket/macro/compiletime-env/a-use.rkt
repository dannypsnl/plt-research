#lang racket/base

(require "a-extra.rkt")

my-extra
(my-extra 2 3)

(require (for-syntax racket/base
                     syntax/parse))
(define-syntax (expand-to-extra-info stx)
  (syntax-parse stx
    [(_ id)
     (define-values (info target)
       (syntax-local-value/immediate #'id))
     #`(quote #,(extrainfo-data info))]))
(expand-to-extra-info my-extra)
