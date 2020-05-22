#lang typed/racket

(provide typ
         typ:builtin
         typ:freevar subst!
         typ:constructor
         typ:arrow)

(struct typ [] #:transparent)
(struct typ:freevar typ
  [(index : Integer)
   (substituted : (Option typ))]
  #:transparent
  #:mutable)
(struct typ:constructor typ
  [(name : String)
   (arg : (Listof typ))]
  #:transparent)
(: typ:builtin (-> String typ:constructor))
(define (typ:builtin name)
  (typ:constructor name '()))
(struct typ:arrow typ [(from : typ) (to : typ)] #:transparent)

(: subst! (-> typ:freevar typ Void))
(define (subst! fv s)
  (let ([ns (match s
              ([typ:freevar _ s] s)
              (s s))])
    (set-typ:freevar-substituted! fv ns)))
