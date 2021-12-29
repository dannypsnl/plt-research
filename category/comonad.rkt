#lang racket

(define (cutter n)
  (lambda (l)
    (split-at l n)))
(define (condition n)
  (lambda (l)
    (>= (length l) 2)))

(define (f c? cut)
  (define (f* l)
    (if (c? l)
        (let-values ([(l rest) (cut l)])
          (cons l (f* rest)))
        null))
  f*)

(define (chunk-by l) ((f (condition 2) (cutter 2)) l))
(chunk-by '(a b c d e))
