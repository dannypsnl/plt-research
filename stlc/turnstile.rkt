#lang turnstile+

(provide (type-out →) λ #%app ann #%module-begin #%top-interaction require #%datum
         Int String)

; (→ typ/param* ... typ/return)
(define-type-constructor →
  #:arity >= 1
  #:arg-variances
  (λ (stx)
    (syntax-parse stx
      [(_ typ/param* ... typ/return)
       (append
        (stx-map (λ _ contravariant) #'[typ/param* ...])
        (list covariant))])))

(define-typed-syntax λ
  #:datum-literals (:)
  ;;; typed form: (λ ([x : Nat]) x)
  [(λ ([x:id : typ/param*:type] ...) e) ≫
                                        [[x ≫ x- : typ/param*] ... ⊢ e ≫ e- ⇒ typ/return]
                                        ---------
                                        [⊢ (λ- (x- ...) e-) ⇒ (→ typ/param* ... typ/return)]]
  ;;; inferred form: (λ (x) x)
  [(λ (x:id ...) e) ⇐ (~→ typ/param* ... typ/return) ≫
                    [[x ≫ x- : typ/param*] ... ⊢ e ≫ e- ⇐ typ/return]
                    ---------
                    [⊢ (λ- (x- ...) e-)]])

(define-typed-syntax (#%app exp/func exp/arg* ...) ≫
  ; infer type of func, produces a arrow type
  [⊢ exp/func ≫ exp/func- ⇒ (~→ typ/arg* ... typ/return)]
  ; ensure exp and type length matched
  ; if fail, use `num-args-fail-msg` constructs error message
  #:fail-unless (stx-length=? #'[typ/arg* ...] #'[exp/arg* ...])
  (num-args-fail-msg #'exp/func #'[typ/arg* ...] #'[exp/arg* ...])
  ; check(⇐) arg*
  [⊢ exp/arg* ≫ exp/arg*- ⇐ typ/arg*] ...
  ---------
  ; synth(⇒) typ/return via application
  [⊢ (#%app- exp/func- exp/arg*- ...) ⇒ typ/return])

(define-typed-syntax (ann e (~datum :) type:type) ≫
  ; check, ⇐ e : type
  [⊢ e ≫ e- ⇐ type.norm]
  ---------
  ; synth, ⇒ e : type
  [⊢ e- ⇒ type.norm])

;;; extension
(define-base-types Int String)
(define-typed-syntax #%datum
  ; an integer literal synth type Int
  [(_ . n:integer) ≫
   ---------
   [⊢ (#%datum- . n) ⇒ Int]]
  ; an string literal synth type String
  [(_ . s:string) ≫
   ---------
   [⊢ (#%datum- . s) ⇒ String]]
  ; unknown literal
  [(_ . x) ≫
   ---------
   [#:error (type-error #:src #'x #:msg "unknown: ~v" #'x)]])
