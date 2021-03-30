#lang racket

(require syntax/parse/define
         (for-syntax racket/match))

(begin-for-syntax
  (define (-> . any)
    `(-> ,@any))

  (define-syntax-class type
    #:datum-literals (->)
    (pattern name:id)
    (pattern (name:id e*:type ...))
    (pattern (-> param*:type ... ret:type)))

  (define-syntax-class constructor
    #:datum-literals (:)
    (pattern (name:id : typ:type)
             #:attr val
             (match (syntax->datum #'typ)
               [`(-> ,param* ... ,ret)
                (define new-param*
                  (generate-temporaries param*))
                #`(λ #,new-param* `(name ,#,@new-param*))]
               [ty #''name])
             #:attr def
             #'(define name val))))

(define-syntax-parser data
  [(_ name:id ctor*:constructor ...)
   #'(begin
       (define-for-syntax name 'name)
       (define-for-syntax ctor*.name ctor*.typ) ...
       ctor*.def ...)])

(module change-app racket
  (provide (except-out (all-from-out racket)
                       #%app)
           (rename-out [@#%app #%app]))

  (require syntax/parse/define
           (for-syntax racket/match))

  (define-for-syntax (<-type stx)
    (syntax-parse stx
      [(f arg* ...)
       (match (eval #'f)
         [`(-> ,param* ... ,ret)
          (for ([arg (syntax->list #'(arg* ...))]
                [exp param*])
            (define act (<-type arg))
            (unless (equal? exp act)
              (raise-syntax-error
               'semantic
               (format "type mismatched: ~a ~a" exp act)
               arg)))
          ret]
         [ty (raise-syntax-error
              'semantic
              (format "cannot apply ~a" ty)
              #'f)])]
      [e (eval #'e)]))

  (define-syntax-parser @#%app
    [(_ f:expr arg*:expr ...)
     (<-type #'(f arg* ...))
     #'(#%app f arg* ...)]))

(require 'change-app)

(module+ main
  (data Bool
        [true : Bool]
        [false : Bool])
  (data Nat
        [zero : Nat]
        [suc : (Nat . -> . Nat)])

  false
  true
  zero
  (suc (suc (suc zero))))
