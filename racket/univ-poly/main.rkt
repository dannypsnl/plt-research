#lang typed/racket

(require "data.rkt")

(define-type Name String)
(define-type Clos (-> Val Val))
(define-type Env (Listof Val))
(define-type Ty Tm)
(define-type RTy RTm)
(define-type VTy Val)
(define-type Ix Integer)
(define-type Lvl Integer)

(data RTm
      [RVar Name]
      [RApp RTm RTm]
      [RLam Name RTm]
      [RPi Name RTm RTm]
      [RLet Name RTy RTm RTm]
      [RUFin RTm]
      [RFinLvl]
      [RL0]
      [RLS RTm]
      [RLMax RTm RTm])

(data Level
      [Fin Tm]
      [Omega])

(data Tm
      [Var Ix]
      [App Tm Tm]
      [Lam Name Tm]
      [Pi Name Tm Tm]
      [Let Name Ty Tm Tm]
      [U Level]
      [FinLvl]
      [L0]
      [LS Tm]
      [LMax Tm Tm])

(data VLevel
      [VFin Val]
      [VOmega])

(data Val
      [VVar Lvl]
      [VApp Val Val]
      [VLam Name Clos]
      [VPi Name VTy Clos]
      [VU VLevel]
      [VFinLvl]
      [VL0]
      [VLS Val]
      [VLMax Val Val])

(: finmax : Val Val -> Val)
(define/match (finmax l1 l2)
  [{(VL0) l2} l2]
  [{l1 (VL0)} l1]
  [{(VLS l1) (VLS l2)} (finmax l1 l2)]
  [{l1 l2} (VLMax l1 l2)])

(: level : Env Level -> VLevel)
(define (level env l)
  (match l
    [(Omega) (VOmega)]
    [(Fin t) (VFin (eval env t))]))

(: eval : Env Tm -> Val)
(define (eval env tm)
  (match tm
    [(Var x) (list-ref env x)]
    [(App t u)
     (match* {(eval env t) (eval env u)}
       [{(VLam _ t) u} (t u)]
       [{t u} (VApp t u)])]
    [(Lam x t)
     (VLam x (λ (u) (eval (cons u env) t)))]
    [(Pi x a b)
     (VPi x (eval env a)
          (λ (u) (eval (cons u env) b)))]
    [(Let x a t u)
     (eval (cons (eval env t) env) u)]
    [(U t)
     (VU (level env t))]
    [(FinLvl) (VFinLvl)]
    [(L0) (VL0)]
    [(LS t) (VLS (eval env t))]
    [(LMax t u) (finmax (eval env t) (eval env u))]))

(: quoteLevel : Lvl VLevel -> Level)
(define (quoteLevel l lvl)
  (match lvl
    [(VFin t) (Fin (quote l t))]
    [(VOmega) (Omega)]))

(: quote : Lvl Val -> Tm)
(define (quote l v)
  (match v
    [(VVar x) (Var (- l x 1))]
    [(VApp t u) (App (quote l t) (quote l u))]
    [(VLam x t) (Lam x (quote (add1 l) (t (VVar l))))]
    [(VPi x a b) (Pi x (quote l a) (quote (add1 l) (b (VVar l))))]
    [(VL0) (L0)]
    [(VLS t) (LS (quote l t))]
    [(VLMax t u) (LMax (quote l t) (quote l u))]
    [(VFinLvl) (FinLvl)]
    [(VU t) (U (quoteLevel l t))]))

(: nf : Tm -> Tm)
(define (nf tm)
  (quote 0 (eval (list) tm)))

(: conv : Lvl Val Val -> Boolean)
(define (conv l v1 v2)
  (match* {v1 v2}
    [{(VVar x) (VVar y)} (= x y)]
    [{(VApp t u) (VApp t2 u2)}
     (and (conv l t t2)
          (conv l u u2))]
    [{(VLam x t) (VLam _ t2)}
     (conv (add1 l) (t (VVar l)) (t2 (VVar l)))]
    [{(VLam x t) t2}
     (conv (add1 l) (t (VVar l)) (VApp t2 (VVar l)))]
    [{t (VLam x t2)}
     (conv (add1 l) (VApp t (VVar l)) (t2 (VVar l)))]
    [{(VPi x a b) (VPi _ a2 b2)}
     (and (conv l a a2)
          (conv (add1 l) (b (VVar l)) (b2 (VVar l))))]
    [{(VL0) (VL0)} #t]
    [{(VLS t) (VLS t2)} (conv l t t2)]
    [{(VLMax t u) (VLMax t2 u2)}
     (and (conv l t t2)
          (conv l u u2))]
    [{(VU (VOmega)) (VU (VOmega))} #t]
    [{(VU (VFin t)) (VU (VFin t2))}
     (conv l t t2)]
    [{(VFinLvl) (VFinLvl)} #t]
    [{_ _} #f]))
