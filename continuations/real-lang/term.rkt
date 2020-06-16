#lang typed/racket

(provide (all-defined-out))

(define-type term
  (U term:var
     term:λ
     term:ap
     term:int-literal
     term:call/cc term:call/ec
     term:if))

(struct term:var
  ([x : String])
  #:transparent)
(struct term:λ
  ([params : (Listof String)]
   [body : term])
  #:transparent)
(struct term:int-literal
  ([v : Integer])
  #:transparent)
(struct term:call/cc () #:transparent)
(struct term:call/ec () #:transparent)
(struct term:if
  ([condition : term]
   [then : term]
   [else : term])
  #:transparent)
;;; application
(struct term:ap
  ([func : term]
   [arg : (Listof term)])
  #:transparent)
