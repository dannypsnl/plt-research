#lang typed/racket

(provide typ
         typ:builtin
         typ:freevar subst
         typ:constructor
         typ:arrow)

(struct typ [] #:transparent)
(struct typ:builtin typ [(name : String)] #:transparent)
(struct typ:freevar typ
  [(index : Integer)
   (substituted : (Option typ))]
  #:transparent
  #:mutable)
(struct typ:constructor typ
  [(name : String)
   (arg : (Listof typ))]
  #:transparent)
(struct typ:arrow typ [(from : typ) (to : typ)] #:transparent)

(: subst (-> typ:freevar typ typ:freevar))
(define (subst freevar substituted)
  (set-typ:freevar-substituted! freevar substituted)
  freevar)
