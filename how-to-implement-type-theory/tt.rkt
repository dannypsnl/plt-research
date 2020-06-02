#lang typed/racket

(provide ty
         (struct-out exp)
         (struct-out exp:var)
         (struct-out exp:typ)
         (struct-out exp:prod)
         (struct-out exp:lambda)
         (struct-out exp:app)
         (struct-out exp:ascribe)
         (struct-out stmt)
         (struct-out stmt:def)
         (struct-out stmt:check)
         (struct-out stmt:eval)
         (struct-out stmt:axiom))

(define-type ty exp)
(struct exp () #:transparent)
(struct exp:var exp ([idx : Integer]) #:transparent)
(struct exp:typ exp () #:transparent)
(struct exp:prod exp
  ([x : (Pair String ty)]
   [m : ty])
  #:transparent)
(struct exp:lambda exp
  ([x : (Pair String (Option ty))]
   [m : exp])
  #:transparent)
(struct exp:app exp
  ([e1 : exp]
   [e2 : exp])
  #:transparent)
(struct exp:ascribe exp
  ([e : exp]
   [t : ty])
  #:transparent)

(struct stmt () #:transparent)
(struct stmt:def stmt
  ([name : String]
   [expr : exp])
  #:transparent)
(struct stmt:check stmt ([e : exp]) #:transparent)
(struct stmt:eval stmt ([e : exp]) #:transparent)
(struct stmt:axiom stmt
  ([name : String]
   [e : exp])
  #:transparent)
