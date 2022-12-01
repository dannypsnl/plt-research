#lang typed/racket/optional
(provide (all-defined-out))
(define-type Name Symbol)

(struct Univ ()
  #:property prop:custom-write
  (λ (_v port _mode)
    (fprintf port "𝕌"))#:transparent)
(struct Var ([x : Name])
  #:property prop:custom-write
  (λ (v port _mode)
    (match-define (Var x) v)
    (fprintf port "~a" x))
  #:transparent)
(struct App ([f : Tm] [arg : Tm])
  #:property prop:custom-write
  (λ (v port _mode)
    (match-define (App t u) v)
    (fprintf port "~a ~a" t u))
  #:transparent)
(struct Lam ([x : Name] [body : Tm])
  #:property prop:custom-write
  (λ (v port _mode)
    (match-define (Lam x t) v)
    (fprintf port "λ~a.~a" x t))
  #:transparent)
(struct Pi ([x : Name] [x-ty : Ty] [body : Ty])
  #:property prop:custom-write
  (λ (v port _mode)
    (match-define (Pi x a b) v)
    (if (eq? x '_)
        (fprintf port "~a → ~a" a b)
        (fprintf port "(~a : ~a) → ~a" x a b)))
  #:transparent)
(struct Let ([x : Name] [a : Ty] [t : Tm] [body : Tm])
  #:property prop:custom-write
  (λ (v port _mode)
    (match-define (Let x a t u) v)
    (fprintf port "let ~a : ~a = ~a;~n~a" x a t u))
  #:transparent)
(define-type Tm (U Univ Var App Lam Pi Let))
(define-type Ty Tm)

