#lang racket
(require racket/contract/parametric)

(define/contract (cutter n)
  (integer? . -> . (parametric->/c [A] ((listof A) . -> . (values (listof A) (listof A)))))
  (lambda (l)
    (split-at l n)))
(define/contract (condition n)
  (integer? . -> . (parametric->/c [A] ((listof A) . -> . boolean?)))
  (lambda (l)
    (>= (length l) 2)))

(define (f c? cut)
  (parametric->/c
   [A]
   (((listof A) . -> . boolean?)
    ((listof A) . -> . (values (listof A) (listof A)))
    . -> .
    (listof (listof A))))
  (define (f* l)
    (if (c? l)
        (let-values ([(l rest) (cut l)])
          (cons l (f* rest)))
        null))
  f*)

(define chunk-by (f (condition 2) (cutter 2)))
(chunk-by '(a b c d e))
