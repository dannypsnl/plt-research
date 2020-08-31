#lang racket

(require (for-syntax syntax/parse)
         racket/syntax
         syntax/stx)
(require "lang.rkt"
         "semantic.rkt"
         "pretty-print.rkt")

(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         (all-from-out "lang.rkt"))

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

(define-syntax-rule (top-interaction e)
  (begin
    (printf "type:- ~a~n" (pretty-print-typ (type/infer (parse e))))
    (parse e)))

(define-syntax-rule (module-begin e* ...)
  (#%module-begin
   (top-interaction e*) ...))

(module reader syntax/module-reader
  hindley-milner)
