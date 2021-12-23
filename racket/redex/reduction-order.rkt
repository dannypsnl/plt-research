#lang racket
(require redex
         "lambda-calculus.rkt")

(define-extended-language LC+number
  LC
  (E ::= ....
     number
     O)
  (O O1 O-multi)
  (O1 add1 sub1)
  (O-multi + *))

(define r
  (extend-reduction-relation
   r-app
   LC+number #:domain E
   (--> (+ number ...) ,(apply * (term (number ...)))
        "+")
   (--> (* number ...) ,(apply * (term (number ...)))
        "*")
   (--> (add1 number) ,(add1 (term number))
        "add1")
   (--> (sub1 number) ,(sub1 (term number))
        "sub1")))

(define -->r (compatible-closure r LC+number E))

(traces -->r (term ((λ (x) (x x)) (λ (x) (x x)))))
(traces -->r (term ((λ (x) (+ 1 2 3)) ((λ (x) (x x)) (λ (x) (x x))))))
