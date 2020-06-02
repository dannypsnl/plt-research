#lang typed/racket

;;; patterns
(struct pattern () #:transparent)
; variable
(struct pattern:var pattern
  ([name : String]) #:transparent)
; pair pattern: p1,p2
(struct pattern:pair pattern
  ([p1 : pattern]
   [p2 : pattern])
  #:transparent)
; ignore pattern
(struct pattern:ignore pattern () #:transparent)

;;; expression
(struct exp () #:transparent)
(struct exp:λ exp () #:transparent)
(struct exp:var exp () #:transparent)
(struct exp:app exp () #:transparent)
(struct exp:pi exp () #:transparent)
(struct exp:U exp () #:transparent)
(struct exp:pair exp () #:transparent)
(struct exp:car exp () #:transparent)
(struct exp:cdr exp () #:transparent)
(struct exp:sigma exp () #:transparent)
(struct exp:0 exp () #:transparent)
(struct exp:1 exp () #:transparent)
(struct exp:c exp () #:transparent)
(struct exp:fun exp () #:transparent)
(struct exp:d exp () #:transparent)
