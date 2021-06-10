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

(: finmax : (-> Val Val Val))
(define/match (finmax l1 l2)
  [{(VL0) l2} l2]
  [{l1 (VL0)} l1]
  [{(VLS l1) (VLS l2)} (finmax l1 l2)]
  [{l1 l2} (VLMax l1 l2)])
