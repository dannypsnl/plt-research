#lang typed/racket

(define-type context (U empty ext))
(struct empty () #:transparent)
(struct ext ([v : value] [ctx : context]) #:transparent)

(define-type term (U S.var S.lam S.app))
(struct S.var ([v : Nonnegative-Integer])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "#~a"
             (S.var-v v)))
  #:transparent)
(struct S.lam ([binder : term])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "λ.~a"
             (S.lam-binder v)))
  #:transparent)
(struct S.app ([fn : term] [arg : term])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "(~a ~a)"
             (S.app-fn v)
             (S.app-arg v)))
  #:transparent)

(define-type value (U V.lam V.stuck))
(define-type V.stuck (U V.var V.app))
(struct V.var ([v : Nonnegative-Integer])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "@~a"
             (V.var-v v)))
  #:transparent)
(struct V.app ([fn : value] [arg : value])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "~a ~a" (V.app-fn v) (V.app-arg v)))
  #:transparent)
(struct V.lam ([binder : term] [ctx : context])
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "λ.~a"
             (V.lam-binder v)))
  #:transparent)

(: proj : context Nonnegative-Integer -> value)
(define (proj ctx i)
  (match ctx
    ; returns a stuck since we don't always have value in context in lambda calculus
    [(empty) (V.var i)]
    [(ext v ctx) (if (= i 0) v (proj ctx (sub1 i)))]))

(: evaluate : context term -> value)
(define (evaluate ctx t)
  (cond
    [(S.var? t) (proj ctx (S.var-v t))]
    [(S.app? t)
     (let ([v-fn (evaluate ctx (S.app-fn t))]
           [v-arg (evaluate ctx (S.app-arg t))])
       (cond
         [(V.lam? v-fn) (evaluate (ext v-arg (V.lam-ctx v-fn))
                                  (V.lam-binder v-fn))]
         [else (V.app v-fn v-arg)]))]
    [(S.lam? t) (V.lam (S.lam-binder t) ctx)]))

(: depth : Sexp -> Nonnegative-Integer)
(define (depth t)
  (match t
    [`(λ (,x) ,body) (add1 (depth body))]
    [`(,f ,a) (max (depth f) (depth a))]
    [else 0]))
(: convert : (->* (Sexp) ((Immutable-HashTable Symbol Nonnegative-Integer))
                  term))
(define (convert t [name->index (make-immutable-hash '())])
  (match t
    [`(λ (,x) ,body)
     (S.lam (convert body (hash-set name->index (cast x Symbol) (depth body))))]
    [`(,t1 ,t2) (S.app (convert t1 name->index) (convert t2 name->index))]
    [x (S.var (hash-ref name->index (cast x Symbol)))]))

(module+ test
  (require typed/rackunit)

  (check-equal? (evaluate (empty)
                          (convert '(a b)
                                   (make-immutable-hash '((a . 1)
                                                          (b . 2)))))
                (V.app (V.var 1) (V.var 2)))
  (check-equal? (evaluate (empty)
                          (convert '(((λ (v) (λ (f) (f v))) k)
                                     (λ (x) x))
                                   (make-immutable-hash '((k . 100)))))
                (V.var 100)))
