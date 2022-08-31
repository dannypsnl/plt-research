#lang racket
(provide (except-out (all-from-out racket) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))
(require (for-syntax syntax/parse
                     syntax/stx
                     racket/pretty
                     "parser.rkt"
                     "semantic.rkt"
                     "pretty-print.rkt"))

(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ e) (pretty-write (syntax->datum #'e) #:newline? #f)
           (printf " :- ~a~n"
                   (pretty-print-typ (type/infer (parse #'e))))
           #'e]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ e* ...)
     #'(#%module-begin
        (top-interaction e*) ...)]))

(module reader syntax/module-reader
  hindley-milner)
