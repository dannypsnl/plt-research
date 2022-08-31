#lang typed/racket
(provide (all-defined-out))

;;; expressions
(struct Expr [] #:transparent)
(struct expr:int Expr [(v : Integer)] #:transparent)
(struct expr:bool Expr [(v : Boolean)] #:transparent)
(struct expr:string Expr [(v : String)] #:transparent)
(struct expr:list Expr [(elems : (Listof Expr))] #:transparent)
(struct expr:variable Expr [(name : String)] #:transparent)
(struct expr:lambda Expr
  [(param : (Listof String))
   (body : Expr)]
  #:transparent)
(struct expr:application Expr
  [(func : Expr)
   (args : (Listof Expr))]
  #:transparent)
(struct expr:let Expr
  [(bindings : (Listof (Pair String Expr)))
   (Expr : Expr)]
  #:transparent)
