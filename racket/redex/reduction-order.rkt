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
  (reduction-relation
   LC+number #:domain E
   (--> ((λ (X ...) E_body) E_args ...)
        (subst ,(map list (term (E_args ...)) (term (X ...)))
               E_body)
        "app")
   (--> (+ number ...) ,(apply * (term (number ...)))
        "+")
   (--> (* number ...) ,(apply * (term (number ...)))
        "*")
   (--> (add1 number) ,(add1 (term number))
        "add1")
   (--> (sub1 number) ,(sub1 (term number))
        "sub1")))

(apply-reduction-relation r (term ((λ (e) e) 1)))

(define -->r (compatible-closure r LC+number E))

(traces -->r (term ((λ (x) (x x)) (λ (x) (x x)))))
(traces -->r (term ((λ (x) 3) ((λ (x) (x x)) (λ (x) (x x))))))
