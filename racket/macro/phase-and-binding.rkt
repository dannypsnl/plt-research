#lang racket

; phase 0
(define age 3)
(begin-for-syntax
  ; phase 1
  (define age 9))

(eval (with-syntax ([age #'age])
        ; phase 0 age
        #'(displayln age)))
(eval (with-syntax ([age #'age])
        ; phase 1 age
        #'(begin-for-syntax
            (displayln age))))
