#lang typed/racket

(provide typ
         typ:builtin
         typ:freevar
         typ:constructor
         typ:arrow)

(struct typ [] #:transparent)
(struct typ:builtin typ [(name : String)] #:transparent)
(struct typ:freevar typ [(index : Integer)] #:transparent)
(struct typ:constructor typ
  [(name : String)
   (arg : (Listof typ))]
  #:transparent)
(struct typ:arrow typ [(from : typ) (to : typ)] #:transparent)
