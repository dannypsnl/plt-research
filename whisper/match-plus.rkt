#lang racket
(provide match+)
(require syntax/parse/define)

(define-syntax-parser match+
  #:datum-literals (=>)
  [(_ v* ...+ #:with
      [p* ...+ => body* ...]
      ...)
   #'(match* {v* ...}
       [(p* ...) body* ...]
       ...)])