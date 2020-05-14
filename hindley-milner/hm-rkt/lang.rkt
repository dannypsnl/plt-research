#lang typed/racket

;;; expressions
(struct expr [])
(struct expr:int expr [(v : Integer)])
(struct expr:bool expr [(v : Boolean)])
(struct expr:string expr [(v : String)])
(struct expr:list expr [(elems : (Listof expr))])
(struct expr:variable expr [(name : String)])
(struct expr:lambda expr
  [(param : (Listof String))
   (body : expr)])
(struct expr:application expr
  [(func : expr)
   (args : (Listof expr))])
(struct expr:let expr
  [(bindings : (Listof (Pair String expr)))
   (expr : expr)])
