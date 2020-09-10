#lang racket

(require racket/set)

(define (ty-> expected actual)
  (match* (expected actual)
    [(`(record ,e-f* ... ,e-p) `(record ,a-f* ...))
     (let/cc return
       (let* ([exp (list->set e-f*)]
              [act (list->set a-f*)]
              [new-set (set-union exp act)])
         (if (empty? e-p)
             (set=? new-set exp)
             (let ([rest (set-subtract act exp)])
               (hash e-p (set->list rest))))))]
    [(_ _) (equal? expected actual)]))

(define (subst t subst-map)
  (match t
    [`(record ,e-f* ... ,e-p)
     `(record ,@e-f* ,(hash-ref subst-map e-p e-p))]
    [else t]))

(define (<-ty tm [env #hash()])
  (match tm
    [`(record (,field-name* ,field-e*) ...)
     `(record ,@(map (λ (name ty)
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
        (define subst? (ty-> arg-ty (<-ty e env)))
        (unless subst?
          (error 'semantic "type mismatching, expected: `~a` got: `~a`" arg-ty (<-ty e env)))
        (if (hash? subst?)
            (subst ret-ty subst?)
            ret-ty)]
       [ft (error 'semantic "not appliable: ~a" ft)])]
    [else (error 'semantic "unknown term: ~a" tm)]))

(<-ty '((λ (e : (record [a : String] [b : Number])) e)
        (record (a "") (b 2))))
#;(<-ty '((λ (e : (record [a : String])) e)
          (record (a "") (b 2))))
(<-ty '((λ (e : (record [a : String] rest)) e)
        (record (a "") (b 2))))
