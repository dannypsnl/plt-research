#lang racket/base

(provide env-ref env-add!
         check-app infer)

(require syntax/parse
         racket/match)

(define id-table (make-hash))
(define (env-add! id value)
  (hash-set! id-table id value))
(define (env-ref id)
  (hash-ref id-table id #f))

(env-add! 'Bool 'Type)
(env-add! 'true 'Bool)
(env-add! 'false 'Bool)

(env-add! 'Nat 'Type)
(env-add! 'zero 'Nat)
(env-add! 'suc '(-> Nat Nat))

(env-add! 'Vec '(-> Type Nat Type))

(define (check-app e)
  (syntax-parse e
      [(f e* ...)
       (define f-ty (infer #'f))
       (define e-ty* (map infer
                          (syntax->list #'(e* ...))))
       (match f-ty
         [`(-> ,t1 ... ,t2)
          (unless (= (length e-ty*) (length t1))
            (raise-syntax-error 'arity
                                ""
                                #'f))
          (for ([e (syntax->list #'(e* ...))]
                [e-ty e-ty*]
                [t t1])
            (unless (equal? t e-ty)
              (raise-syntax-error 'type-mismatched
                                  (format "expect: ~a, get: ~a" t e-ty)
                                  #'f
                                  e)))
          t2]
         ; in this case, means this is builtin method, we do nothing
         [#f (void)]
         [else (raise-syntax-error 'not-func
                                   ""
                                   #'f)])]))
(define (infer e)
  (syntax-parse e
    [(f e ...)
     (check-app #'(f e ...))]
    [e:id (env-ref (syntax->datum #'e))]))
