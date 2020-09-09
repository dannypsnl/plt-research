#lang racket

(require racket/hash)

(define (ty-> expected actual)
  (match* (expected actual)
    [(`(record (,e-f* ...) ,e-p) `(record ,a-f*))
     (let/cc return
       (let* ([exp (make-immutable-hash e-f*)]
              [act (make-immutable-hash a-f*)]
              [new-set (hash-union exp act
                                   #:combine/key (lambda (k v1 v2) (if (equal? v1 v2) v2 (return #f))))])
         (if (empty? e-p)
             (= (hash-count new-set) (hash-count exp))
             #t)))]
    [(_ _) (equal? expected actual)]))

(define (<-ty tm [env #hash()])
  (match tm
    [`(record (,field-name* ,field-e*) ...)
     `(record ,(map (λ (name ty)
                      `(,name : ,ty))
                    field-name*
                    (map (λ (e) (<-ty e env)) field-e*)))]
    [`(,lam (,arg : ,ty) ,b) #:when (member lam '(λ lambda))
     `(-> ,ty ,(<-ty b (hash-set env arg ty)))]
    [id #:when (symbol? id)
        (hash-ref env id (λ () (error 'semantic "no identifier: ~a" id)))]
    [num #:when (number? num) 'Number]
    [str #:when (string? str) 'String]
    [`(,f ,e)
     (match (<-ty f env)
       [`(-> ,arg-ty ,ret-ty)
        ;;; TODO: unification to remove row-poly free variables
        (unless (ty-> arg-ty (<-ty e env))
          (error 'semantic "type mismatching, expected: `~a` got: `~a`" arg-ty (<-ty e env)))
        ret-ty]
       [ft (error 'semantic "not appliable: ~a" ft)])]
    [else (error 'semantic "unknown term: ~a" tm)]))

(<-ty '((λ (e : (record ([a : String] [b : Number]))) e)
        (record (a "") (b 2))))
#;(<-ty '((λ (e : (record ([a : String]))) e)
          (record (a "") (b 2))))
(<-ty '((λ (e : (record ([a : String]) rest)) e)
        (record (a "") (b 2))))
