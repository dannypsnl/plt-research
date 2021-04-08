#lang typed/racket

(provide data)

(require syntax/parse/define)

(begin-for-syntax
  (define-syntax-class binding
    #:datum-literals (:)
    (pattern (name:id : ty)))
  (define-syntax-class constructor
    (pattern (name:id b*:binding ...))))

(define-syntax-parser data
  [(_ name:id c*:constructor ...)
   #'(begin
       (struct name () #:transparent)
       (struct c*.name name (c*.b* ...) #:transparent) ...)])
