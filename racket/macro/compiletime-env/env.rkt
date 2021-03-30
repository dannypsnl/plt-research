#lang racket/base

(require racket/list
         syntax/id-table)

(provide env-ref env-has-id? env-add!)

(define id-table (make-free-id-table))

(define (env-ref id)
  (debug-print-binding-info 'env-ref id)
  (free-id-table-ref id-table id))

(define (env-has-id? id)
  (debug-print-binding-info 'env-has-id? id)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (free-id-table-ref id-table id)
    #t))

(define (env-add! id value)
  (debug-print-binding-info 'env-add! id)
  (free-id-table-set! id-table id value))

(define (debug-print-binding-info who id)
  (define binding
    (identifier-binding id))
  (printf "env.rkt: ~a:\n  id: ~s\n  binding: ~s\n"
          who
          id
          (and (list? binding) (take binding 2))))
