#lang racket
(require redex)

(define-language Arith
  (E ::=
     N O X
     (E E ...))
  (X ::= variable-not-otherwise-mentioned)
  (N ::= number)
  (O ::= O1 O2)
  (O1 ::= add1 sub1)
  (O2 ::= + *))

(define r
  (reduction-relation
   Arith #:domain E
   (--> (O N_0 ...) N_1
        (judgment-holds (Reduce (O N_0 ...) N_1))
        "Reduce")))

(define-judgment-form Arith
  #:mode (Reduce I O)
  #:contract (Reduce (O N ...) N)
  [(Reduce (+ N_0 N_1) ,(+ (term N_0) (term N_1)))]
  [(Reduce (* N_0 N_1) ,(* (term N_0) (term N_1)))]
  [(Reduce (sub1 N) ,(sub1 (term N)))]
  [(Reduce (add1 N) ,(add1 (term N)))])

(define -->r (compatible-closure r Arith E))
(traces -->r (term (+ (add1 5) (* 2 3))))
