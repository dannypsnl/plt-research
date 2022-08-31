#lang hindley-milner

;;; integer
1
;;; bool
#t
#f
;;; string
"abc"
;;; list
'(1 2 3)
;;; lambda
(λ (a) "")
(λ () #t)
(λ (a b) 1)
;;; let binding
(let ([a 1]
      [b (λ (x) x)])
  (b a))
