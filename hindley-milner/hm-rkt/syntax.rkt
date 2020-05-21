#lang racket

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt")

(provide #%module-begin
         #%top-interaction
         (rename-out [hm:lambda lambda]
                     [hm:datum #%datum]))

;;; (λ (a b c) a)
(define-syntax (hm:lambda stx)
  (syntax-parse stx
    ([_ (params*:id ...) body]
     #'(expr:lambda (list (symbol->string 'params*) ...) body))))

(define-syntax (hm:datum stx)
  (syntax-parse stx
    ([_ . s:string] #'(expr:string (#%datum . s)))
    ([_ . b:boolean] #'(expr:bool (#%datum . b)))
    ([_ . i:exact-integer] #'(expr:int (#%datum . i)))))
