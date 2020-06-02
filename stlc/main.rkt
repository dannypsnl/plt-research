#lang typed/racket

;;; ty
(struct ty () #:transparent)
(struct ty:primitive ty ([name : String]) #:transparent)
(struct ty:→ ty
  ([t1 : ty]
   [t2 : ty]) #:transparent)
;;; term
(struct term () #:transparent)
(struct term:var term ([name : String]) #:transparent)
(struct term:λ term
  ([x : String]
   [xt : ty]
   [m : term]) #:transparent)
(struct term:app term
  ([f : term]
   [a : term]) #:transparent)
;;; context
(struct context
  ([name→ty : (Mutable-HashTable String ty)]
   [parent : (Option context)])
  #:transparent #:mutable)
(define (context/new [parent : (Option context) #f])
  (context (make-hash '())
           parent))
(define (context-bind! [ctx : context]
                       [name : String]
                       [typ : ty])
  : Void
  (let ([env (context-name→ty ctx)])
    (if (hash-has-key? env name)
        (raise (format "redefined: ~a" name))
        (hash-set! env name typ))))
(define (context-lookup [ctx : context]
                        [name : String])
  : ty
  (define (lookup-parent)
    (define p : (Option context) (context-parent ctx))
    (if p
        (context-lookup p name)
        (raise (format "no variable named: ~a" name))))
  (hash-ref (context-name→ty ctx) name
            ; lookup parent if failed in current level
            lookup-parent))
;;; ty-check
(: ty-check (->* (term) (context) ty))
(define (ty-check e [ctx (context/new)])
  (match e
    [(term:var name) (context-lookup ctx name)]
    [(term:λ x xt m)
     (define λ-ctx : context (context/new ctx))
     (context-bind! λ-ctx x xt)
     (ty:→ xt (ty-check m λ-ctx))]
    [(term:app e1 e2)
     (match (ty-check e1 ctx)
       [(ty:→ t1 t2)
        (if (equal? t1 (ty-check e2 ctx))
            t2
            (raise (format "type mismatching, expected: ~a, but got: ~a" t1 (ty-check e2 ctx))))]
       [_ (raise "not function")])]))

(module+ test
  (require typed/rackunit))
(module+ test
  (define ctx (context/new))
  (context-bind! ctx "a" (ty:primitive "int"))
  (check-equal?
   (ty-check (term:app (term:λ "x" (ty:primitive "int") (term:var "x"))
                    (term:var "a"))
          ctx)
   (ty:primitive "int")))
