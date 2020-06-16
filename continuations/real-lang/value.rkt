#lang typed/racket

(provide (all-defined-out))

(define-type atom-value
  (U atom:var
     atom:λ
     atom:int-literal))

(struct atom:var
  ([x : String])
  #:transparent)
(struct atom:λ
  ([params : (Listof String)]
   [body : complex-value]))
(struct atom:int-literal
  ([v : Integer])
  #:transparent)

(define-type complex-value
  (U complex:ap
     complex:if))

(struct complex:ap
  ([func : atom-value]
   [args : (Listof atom-value)])
  #:transparent)
(struct complex:if
  ([condition : atom-value]
   [then : complex-value]
   [else : complex-value])
  #:transparent)
