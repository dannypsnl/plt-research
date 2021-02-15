#lang racket

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ name:id)
     (displayln (syntax-debug-info stx))
     #''bar]))

(letrec-values ()
  (foo a))
