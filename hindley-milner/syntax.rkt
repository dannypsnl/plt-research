#lang racket

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt")

(provide #%module-begin
         #%top-interaction
         (rename-out [hm:lambda λ]
                     [hm:app #%app]
                     [hm:datum #%datum]
                     [hm:let let]
                     [hm:identifier #%top]))

;;; (λ (a b c) a)
(define-syntax (hm:lambda stx)
  (syntax-parse stx
    ([_ (params*:id ...) body]
     #'(expr:lambda (list (symbol->string 'params*) ...) body))))

;;; (let ([a 1] [b (λ (x) x)])
;;;   (b a))
(define-syntax (hm:let stx)
  (define-syntax-class bind
    (pattern (bind-name:id bind-expr)
             #:with bind
             #'(cons (symbol->string 'bind-name) bind-expr)))
  (syntax-parse stx
    ([_ (binding*:bind ...) body]
     #'(expr:let (list binding*.bind ...) body))))

;;; (f arg)
(define-syntax (hm:app stx)
  (syntax-parse stx
    ([_ f arg* ...]
     #'(expr:application f (list arg* ...)))))

(define-syntax (hm:identifier stx)
  (syntax-parse stx
    ([_ . v:id] #'(expr:variable (symbol->string 'v)))))
(define-syntax (hm:datum stx)
  (syntax-parse stx
    ([_ . s:string] #'(expr:string (#%datum . s)))
    ([_ . b:boolean] #'(expr:bool (#%datum . b)))
    ([_ . i:exact-integer] #'(expr:int (#%datum . i)))))
