#lang racket
(require racket/contract/parametric)

(define/contract (cutter n)
  (-> integer?
      (parametric->/c
       [A]
       (-> (listof A) (values (listof A) (listof A)))))
  (lambda (l)
    (if (>= (length l) n)
        (split-at l n)
        (values null null))))

(define (f cut)
  (parametric->/c
   [A]
   (((listof A) . -> . (values (listof A) (listof A)))
    . -> .
    (listof (listof A))))
  (lambda (l)
    (let-values ([(l rest) (cut l)])
      (if (null? l)
          rest
          (cons l ((f cut) rest))))))

(define chunk-by (f (cutter 2)))
(chunk-by '(a b c d e))
