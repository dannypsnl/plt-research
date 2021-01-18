#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  ;; without begin-for-syntax, the define-syntax-class would define in phase 0(runtime). begin-for-syntax lift it to phase 1
  (define-syntax-class foo
    (pattern (a b c))))

(define-syntax (macro stx)
  (syntax-parse stx
    ;; thus `f:foo` can reference to foo class
    [(_ f:foo) #'(+ f.a f.b f.c)]))
