#lang racket

;;; lambda calculus
; T = X
;   | (λ (X) T)
;   | (T T)
;;; normal form
; NF = NEU
;    | (λ (X) NF)
; NEU = X
;    | (NEU NF)
;;; evaluate : EXP → EXP
(define (evaluate e)
  (match e
    [`(,e1 ,e2)
     (match (evaluate e1)
       [`(lambda (,x) ,e2)
        (evaluate (subst e2 x e1))]
       [e1*
        `(,e1* ,e2)])]
    [e e]))