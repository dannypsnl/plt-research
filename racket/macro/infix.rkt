#lang racket

(module infix racket
  (provide (all-defined-out))

  (require (for-syntax syntax/parse/class/paren-shape)
           (prefix-in racket/base/ racket/base)
           syntax/parse/define)

  (define-syntax-parser #%infix
    [(_ a op b)
     #'(racket/base/#%app op a b)])

  (define-syntax-parser #%app
    [{~braces _ arg ...}
     #'(#%infix arg ...)]
    [(_ arg ...)
     #'(racket/base/#%app arg ...)]))

(require 'infix
         (for-syntax 'infix))

(+ 1 2)
{1 + 2}

(begin-for-syntax
  {1 + 2})
