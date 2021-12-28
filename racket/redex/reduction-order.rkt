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

;(traces -->r (term ((λ (x) (x x)) (λ (x) (x x)))))

(define (layout terms)
  (define root
    (for/or ([term (in-list terms)])
      (and (null? (term-node-parents term))
           term)))

  (define visited (make-hash))
  (let loop ([depth 0]
             [width 0]
             [node root])
    (cond
      [(hash-ref visited node #f) (void)]
      [else
       (hash-set! visited node #t)
       (term-node-set-position! node (* width 200) (* depth 100))
       (for ([child (in-list (term-node-children node))]
             [width (in-naturals)])
         (loop (+ depth 1)
               width
               child))])))

(traces -->r (term ((λ (x) (+ 1 2 3)) ((λ (x) (x x)) (λ (x) (x x)))))
        #:layout layout)
