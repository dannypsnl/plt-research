#lang typed/racket

(provide data)

(require syntax/parse/define
         (for-syntax racket/syntax))

(begin-for-syntax
  (define-syntax-class binding
    #:datum-literals (:)
    (pattern (name:id : ty)
             #:attr pair
             #'(name : ty))
    (pattern ty
             #:attr pair
             #`(#,(generate-temporary) : ty)))
  (define-syntax-class constructor
    (pattern (name:id b*:binding ...)
             #:attr bind* #'(b*.pair ...))))

(define-syntax-parser data
  [(_ name:id c*:constructor ...)
   #'(begin
       (struct name () #:transparent)
       (struct c*.name name c*.bind* #:transparent) ...)])
