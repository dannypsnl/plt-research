#lang typed/racket

(provide (all-defined-out))

(require "value.rkt")

(define-type checkable-term
  (U
   inferable-term
   t:λ))
(define-type inferable-term
  (U
   t:annotation
   t:*
   t:Π
   t:bound
   t:free
   t:app))
(struct t:λ
  ([checkable : checkable-term])
  #:transparent)
(struct t:annotation
  ([c1 : checkable-term]
   [c2 : checkable-term]) #:transparent)
(struct t:* () #:transparent)
(struct t:Π
  ([t1 : checkable-term]
   [t2 : checkable-term]) #:transparent)
(struct t:bound ([i : Integer]) #:transparent)
(struct t:free ([name : name]) #:transparent)
(struct t:app
  ([func : inferable-term]
   [arg : checkable-term]) #:transparent)
