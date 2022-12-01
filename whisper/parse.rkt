#lang racket
(provide parse)
(require syntax/parse
         syntax/stx
         "term.rkt")

(define (parse stx)
  (syntax-parse stx
    #:datum-literals (U
                      λ lam =>
                      : ->
                      let = in)
    [U (Univ)]
    [((~or λ lam) x ...+ => t)
     (foldr (λ (x t) (Lam x t))
            (parse #'t)
            (stx-map syntax->datum #'(x ...)))]
    [(lam x t) (Lam (syntax->datum #'x) (parse #'t))]
    [(let x : a = t in u) (Let (syntax->datum #'x) (parse #'a) (parse #'t) (parse #'u))]
    [((x : a) -> b) (Pi (syntax->datum #'x) (parse #'a) (parse #'b))]
    [(a -> b) (Pi '_ (parse #'a) (parse #'b))]
    [(t u) (App (parse #'t) (parse #'u))]
    [x:id (Var (syntax->datum #'x))]))
