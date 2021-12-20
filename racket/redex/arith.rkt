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
   (--> (+ N ...) ,(apply * (term (N ...))))
   (--> (* N ...) ,(apply * (term (N ...))))
   (--> (add1 N) ,(add1 (term N)))
   (--> (sub1 N) ,(sub1 (term N)))))

(define -->r (compatible-closure r Arith E))
(traces -->r (term (+ (add1 5) (* 2 3 4))))
