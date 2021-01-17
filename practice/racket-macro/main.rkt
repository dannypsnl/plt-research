#lang racket/base

(module phase-mismatch-mod racket
  (require syntax/parse (for-syntax syntax/parse))
  (define-syntax-class foo
    (pattern (a b c)))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))
