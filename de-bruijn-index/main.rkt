#lang typed/racket

(module+ test
  (require typed/rackunit))

(define-type term (U t:var t:λ (app term)))
(define-type t:var Symbol)
(struct t:λ
  ([x : t:var]
   [m : term])
  #:transparent
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "λ~a.~a"
             (t:λ-x v)
             (t:λ-m v))))
(struct (t) app
  ([t1 : t]
   [t2 : t])
  #:transparent
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "(~a ~a)"
             (app-t1 v)
             (app-t2 v))))

(define-type bterm (U b:var b:λ (app bterm)))
(define-type b:var Integer)
(struct b:λ ([m : bterm])
  #:transparent
  #:property prop:custom-write
  (λ (v port mode)
    (fprintf port "λ~a" (b:λ-m v))))

;;; convert lambda calculus to de-bruijn index form
(: convert (->* (term)
                ((Immutable-HashTable Symbol Integer))
                bterm))
(define (convert t [name->index (make-immutable-hash '())])
  (match t
    ;; bind parameter name to an index and keep conversion
    [(t:λ p b) (b:λ (convert b (hash-set name->index p (hash-count name->index))))]
    [(app t1 t2) (app (convert t1 name->index) (convert t2 name->index))]
    ;; get index from environment
    [name (hash-ref name->index name)]))

(module+ test
  (check-equal? (convert (t:λ 'x 'x))
                (b:λ 0))
  (check-equal? (convert (t:λ 'f (app (t:λ 'x (app 'f (app 'x 'x)))
                                      (t:λ 'x (app 'f (app 'x 'x))))))
                (b:λ (app (b:λ (app 0 (app 1 1)))
                          (b:λ (app 0 (app 1 1)))))))

;;; beta-reduction
; if t1 is a λx.M then replace all x occurs in M with t2
; since bterm using de-bruijn index, lifting after beta-reduction is required
; to know which level we are, must count λ
(: β-reduction (->* (bterm)
                    (Integer (Immutable-HashTable Integer bterm))
                    bterm))
(define (β-reduction t [de-bruijn-level 0] [subst (make-immutable-hash '())])
  (match t
    [(b:λ body) (b:λ (β-reduction body (add1 de-bruijn-level) subst))]
    [(app t1 t2)
     (match t1
       [(b:λ body)
        (let ([reduced-term (β-reduction body (add1 de-bruijn-level)
                                         (hash-set subst de-bruijn-level t2))])
          ;;; dbi lifting by replace reduced-term (+ 1 dbi) with (var dbi)
          (β-reduction reduced-term de-bruijn-level
                       (hash-set subst (add1 de-bruijn-level) de-bruijn-level)))]
       [_ (raise "cannot do application on non-lambda term")])]
    [i (hash-ref subst i (λ () t))]))

(module+ test
  ;;; λx.x
  ; => λ0
  ; beta-> λ0
  (check-equal? (β-reduction (convert (t:λ 'x 'x)))
                (b:λ 0))
  ;;; λx.(λy.λz.z x)
  ; => λ(λλ2 0)
  ; beta-> λλ2
  ; beta-> λλ1 (de-bruijn index lifting)
  (check-equal? (β-reduction (convert (t:λ 'x (app (t:λ 'y (t:λ 'z 'z)) 'x))))
                (b:λ (b:λ 1))))
