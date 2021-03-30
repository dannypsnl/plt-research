#lang racket/base

(provide env-ref env-add!
         env-has?)

(require syntax/id-table)

(define id-table (make-free-id-table))

(define (env-ref id)
  (free-id-table-ref id-table id))

(define (env-has? id)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (free-id-table-ref id-table id)
    #t))

(define (env-add! id value)
  (free-id-table-set! id-table id value))
