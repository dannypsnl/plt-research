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
