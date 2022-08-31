#lang racket
(provide parse)
(require syntax/parse
         syntax/stx
         "lang.rkt")

(define (parse stx)
  (syntax-parse stx
    #:datum-literals (let λ quote)
    ; (let ([a 1]
    ;       [b (λ (x) x)])
    ;   (b a))
    [(let ([name:id exp:expr] ...) body)
     (expr:let (stx-map (λ (n e) (cons (syntax->datum n) (parse e)))
                        #'(name ...)
                        #'(exp ...))
               (parse #'body))]
    [(λ (ps* ...) body)
     (expr:lambda (stx-map syntax->datum #'(ps* ...))
                  (parse #'body))]
    [(quote elem*)
     (expr:list (stx-map parse #'elem*))]
    [(f:expr arg*:expr ...)
     (expr:application (parse #'f) (stx-map parse #'(arg* ...)))]
    [v:id (expr:variable (syntax->datum #'v))]
    [s:string (expr:string (syntax->datum #'s))]
    [b:boolean (expr:bool (syntax->datum #'b))]
    [i:exact-integer (expr:int (syntax->datum #'i))]))
