#lang racket
(require redex)

(define-language Arith
  (E ::=
     N O X
     (E E ...))
  (X ::= variable-not-otherwise-mentioned)
  (N ::= number)
  (O ::= O1 O-multi)
  (O1 ::= add1 sub1)
  (O-multi ::= + *))

(define r
  (reduction-relation
   Arith #:domain E
   (--> (O N_0 ...) N_1
        (judgment-holds (Reduce (O N_0 ...) N_1))
        "Reduce")))

(define-judgment-form Arith
  #:mode (Reduce I O)
  #:contract (Reduce (O N ...) N)
  [(Reduce (+ N ...) ,(apply * (term (N ...))))]
  [(Reduce (* N ...) ,(apply * (term (N ...))))]
  [(Reduce (sub1 N) ,(sub1 (term N)))]
  [(Reduce (add1 N) ,(add1 (term N)))])

(define -->r (compatible-closure r Arith E))
(traces -->r (term (+ (add1 5) (* 2 3 4))))
