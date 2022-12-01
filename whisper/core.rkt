#lang typed/racket/optional
(provide infer-empty
         readback)
(require/typed racket/dict
               [dict-ref (∀ (a) ((Listof (Pairof Name a)) Name -> a))]
               [dict-set (∀ (a) ((Listof (Pairof Name a)) Name a -> (Listof (Pairof Name a))))])
(require "term.rkt"
         "val.rkt"
         ;; macro `match+`
         "match-plus.rkt")

(: fresh : Symbol -> Symbol)
(define (fresh v) (if (eq? v '_) v (gensym v)))

(define-type Env (Listof (Pairof Name Val)))
(define-type Ctx (Listof (Pairof Name VTy)))

(: infer-empty : Tm -> VTy)
(define (infer-empty tm) (infer '() '() tm))

(: infer : Env Ctx Tm -> VTy)
(define (infer env ctx tm)
  (match tm
    [(Univ) (VUniv)]
    [(Var x) (dict-ref ctx x)]
    [(App t u) (define t-ty (infer env ctx t))
               (match t-ty
                 [(VPi _ a b)
                  (check env ctx u a)
                  (b (eval env u))]
                 [_ (error 'bad-app)])]
    [(Lam _ _) (error 'cannot-infer-lambda (format "~a" tm))]
    [(Pi x a b) (check env ctx a (VUniv))
                (check (dict-set env x (VVar x))     ;; x := #x
                       (dict-set ctx x (eval env a)) ;; x : a
                       b
                       (VUniv))
                (VUniv)]
    [(Let x a t u) (check env ctx a (VUniv))
                   (define a- (eval env a))
                   (check env ctx t a-)
                   (infer (dict-set env x (eval env t)) ;; x := t
                          (dict-set ctx x a-)           ;; x : a
                          u)]))

(: check : Env Ctx Tm VTy -> Void)
(define (check env ctx tm ty)
  (match+
   tm ty #:with
   [(Lam x t)
    (VPi (app fresh x-) a b) =>
    (check (dict-set env x (VVar x-))
           (dict-set ctx x a)
           t
           (b (VVar x-)))]
   [(Let x a t u)
    _ =>
    (check env ctx a (VUniv))
    (define a- (eval env a))
    (check env ctx t a-)
    (check (dict-set env x (eval env t)) ;; x := t
           (dict-set ctx x a-)           ;; x : a
           u
           ty)]
   [_ _ => (define ty- (infer env ctx tm))
      (unless (conv env ty- ty)
        (error 'type-mismatched))]))

(: eval : Env Tm -> Val)
(define (eval env tm)
  (match tm
    [(Var x) (dict-ref env x)]
    [(App t u) (define u- (eval env u))
               (match (eval env t)
                 [(VLam _ t) (t u-)]
                 [t- (VApp t- u-)])]
    [(Univ) (VUniv)]
    [(Lam x t) (VLam x (λ (u) (eval (dict-set env x u) t)))]
    [(Pi x a b) (VPi x
                     (eval env a)
                     (λ (u) (eval (dict-set env x u) b)))]
    [(Let x _ t u) (eval (dict-set env x (eval env t))
                         u)]))

(: conv : Env Val Val -> Boolean)
(define (conv env a b)
  (match+
   a b #:with
   [(VUniv) (VUniv) => #t]
   [(VPi (app fresh x) a b)
    (VPi _ a- b-) =>
    (and (conv env a a-)
         (conv (dict-set env x (VVar x))
               (b (VVar x))
               (b- (VVar x))))]
   [(VLam (app fresh x) t)
    (VLam _ t-) =>
    (conv (dict-set env x (VVar x))
          (t (VVar x)) (t- (VVar x)))]
   [(VLam (app fresh x) t)
    u =>
    (conv (dict-set env x (VVar x))
          (t (VVar x)) (VApp u (VVar x)))]
   [u
    (VLam (app fresh x) t) =>
    (conv (dict-set env x (VVar x))
          (VApp u (VVar x)) (t (VVar x)))]
   [(VVar a) (VVar b) => (equal? a b)]
   [(VApp t u) (VApp t- u-) => (and (conv env t t-) (conv env u u-))]))

(: readback : Env Val -> Tm)
(define (readback env v)
  (match v
    [(VUniv) (Univ)]
    [(VVar x) (Var x)]
    [(VApp t u) (App (readback env t) (readback env u))]
    [(VLam (app fresh x) t)
     (Lam x
          (readback (dict-set env x (VVar x))
                    (t (VVar x))))]
    [(VPi (app fresh x) a b)
     (Pi x
         (readback env a)
         (readback (dict-set env x (VVar x)) (b (VVar x))))]))
