# hindley milner

This is just a simple(and of course buggy) language for showing how to inference expression to automatically get type.

### Example

```racket
#lang hindley-milner

;;; integer
1
;;; bool
#t
#f
;;; string
"a"
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
```
