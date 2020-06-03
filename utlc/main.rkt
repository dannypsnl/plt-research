#lang racket

;;; untyped lambda calculus
; t = x
;   | (λ (x) t)
;   | (t t)
; x = variable name
;;; subst
(define (subst e x s)
  (match e
    [`(λ (,i) ,b)
     (cond
       [(equal? `,i `,x) `,e]
       [(equal? `,i `,s)
        `(λ (,(gensym `,i)) `,b)]
       [#t `(λ (,i)
              ,(subst b x s))])]
    [`(,e1 ,e2)
     `(,(subst e1 x s) ,(subst e2 x s))]
    [`,e
     (if (equal? `,e `,x)
         `,s
         `,e)]))
;;; utlc : t -> t
(define (utlc t)
  (match t
    [`(λ (,x) ,b)
     `(λ (,x) ,(utlc b))]
    [`(,f ,a)
     (match (utlc f)
       [`(λ (,x) ,b)
        (utlc (subst b x a))]
       [f1
        `(,f1 ,a)])]
    [`,e `,e]))

(module+ test
  (require rackunit))
(module+ test
  (check-equal? (utlc 'a) 'a)
  (check-equal? (utlc '(λ (x) x))
                '(λ (x) x))
  (check-equal? (utlc '((λ (x) x) a))
                'a))
