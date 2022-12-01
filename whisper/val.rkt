#lang typed/racket/optional
(provide (all-defined-out))
(require "term.rkt")

(struct VUniv () #:transparent)
(struct VVar ([x : Name]) #:transparent)
(struct VApp ([f : Val] [arg : Val]) #:transparent)
(struct VLam ([x : Name] [lam : (-> Val Val)]) #:transparent)
(struct VPi ([x : Name] [x-ty : Val] [lam : (-> Val Val)]) #:transparent)
(define-type Val (U VUniv VVar VApp VLam VPi))
(define-type VTy Val)
