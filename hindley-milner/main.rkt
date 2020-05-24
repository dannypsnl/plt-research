#lang racket

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt"
         "semantic.rkt"
         "pretty-print.rkt")

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-syntax (parse stx)
  (define-syntax-class bind
    (pattern (bind-name:id bind-expr)
             #:with bind
             #'(cons (symbol->string 'bind-name) (parse bind-expr))))
  (syntax-parse stx
    ; (let ([a 1]
    ;       [b (λ (x) x)])
    ;   (b a))
    (`[(~literal let) (binding*:bind ...) body]
     #'(expr:let (list binding*.bind ...) (parse body)))
    (`[(~literal λ) (ps* ...) body] #'(expr:lambda (list (symbol->string 'ps*) ...) (parse body)))
    (`[(~literal quote) (elem* ...)] #'(expr:list (list (parse elem*) ...)))
    (`[f arg* ...] #'(expr:application (parse f) (list (parse arg*) ...)))
    (`v:id #'(expr:variable (symbol->string 'v)))
    (`s:string #'(expr:string (#%datum . s)))
    (`b:boolean #'(expr:bool (#%datum . b)))
    (`i:exact-integer #'(expr:int (#%datum . i)))))

(define-syntax-rule (module-begin EXPR ...)
  (#%module-begin
   (define all-form (list (parse EXPR) ...))
   (for-each (λ (form)
               (displayln form)
               (printf "type:- ~a~n" (pretty-print-typ (type/infer form))))
             all-form)))

(define-syntax-rule (top-interaction . exp)
  (pretty-print-typ (type/infer (parse exp))))

(module reader syntax/module-reader
  hindley-milner)
