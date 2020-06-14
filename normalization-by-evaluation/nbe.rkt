#lang racket
;;; NOTE: totally from http://davidchristiansen.dk/tutorials/nbe/
(require (for-syntax syntax/parse))

;;; closure
(struct CLOS
  (env var body)
  #:transparent)

(define (extend env x typ)
  (cons (cons x typ) env))

(define (val env exp)
  (match exp
    [`(the ,type ,expr) (val env expr)]
    ['zero (ZERO)]
    [`(add1 ,n) (ADD1 (val env n))]
    [x #:when (and (symbol? x)
                   (not (memv x '(the zero add1 λ rec))))
       (cdr (assv x env))]
    [`(λ (,x) ,b)
     (CLOS env x b)]
    [`(rec ,type ,target ,base ,step)
     (do-rec type (val env target) (val env base) (val env step))]
    [`(,rator ,rand)
     (do-application
      (val env rator)
      (val env rand))]))

(define (do-application fun arg)
  (match fun
    [(CLOS env x b)
     (val (extend env x arg) b)]
    [(NEU `(→ ,A ,B) ne)
     (NEU B (N-application ne (THE A arg)))]))
(define (do-rec type target base step)
  (match target
    [(ZERO) base]
    [(ADD1 n) (do-application (do-application step n)
                              (do-rec type n base step))]
    [(NEU 'Nat ne)
     (NEU type
          (N-rec type
                 ne
                 (THE type base)
                 (THE `(→ Nat (→ ,type ,type)) step)))]))

(define (run-program Δ exprs)
  (match exprs
    ['() (go Δ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([type (synth (defs->ctx Δ) e)])
            (run-program (extend Δ x (def type (val (defs->env Δ) e)))
                         rest))]
    [(cons e rest)
     (let ([Γ (defs->ctx Δ)]
           [δ (defs->env Δ)])
       (go-on ([type (synth Γ e)])
              (let ([v (val δ e)])
                (begin
                  (printf "(the ~a\n   ~a)\n"
                          type
                          (read-back (map car Γ) type v))
                  (run-program Δ rest)))))]))

(define (add-* x) (string->symbol (string-append (symbol->string x) "*")))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

;;; Expressions equated by zero or more α and β steps are called αβ-equivalent.
; α-renaming and β-reduction
;;; grammar
; normal-form ::= <neutral>
;               | (λ (<id>) <normal-form>)
; neutral ::= <id>
;           | ( <neutral> <normal-form> )
;;; neutral variable
(struct N-var (name) #:transparent)
;;; neutral application
(struct N-application (rator rand))
(struct N-rec (type target base step)
  #:transparent)

(define (read-back used-names type value)
  (match type
    ['Nat
     (match value
       [(ZERO) 'zero]
       [(ADD1 n) `(add1 ,(read-back used-names 'Nat n))]
       [(NEU _ ne)
        (read-back-neutral used-names ne)])]
    [`(→ ,A ,B)
     (let ([x (freshen used-names 'x)])
       `(λ (,x)
          ,(read-back (cons x used-names)
                      B
                      (do-application value (NEU A (N-var x))))))]))
(define (read-back-neutral used-names ne)
  (match ne
    [(N-var x) x]
    [(N-application fun (THE arg-type arg))
     `(,(read-back-neutral used-names fun)
       ,(read-back used-names arg-type arg))]
    [(N-rec type target (THE base-type base) (THE step-type step))
     `(rec ,type
           ,(read-back-neutral used-names target)
           ,(read-back used-names base-type base)
           ,(read-back used-names step-type step))]))

(define (norm env e)
  (read-back '() (val env e)))

(struct go (result) #:transparent)
(struct stop (expr message) #:transparent)

(define-syntax (go-on stx)
  (syntax-parse stx
    [(go-on () result) (syntax/loc stx result)]
    [(go-on ([pat0 e0] [pat e] ...) result)
     (syntax/loc stx
       (match e0
         [(go pat0) (go-on ([pat e] ...) result)]
         [(go v) (error 'go-on "Pattern did not match value ~v" v)]
         [(stop expr msg) (stop exp msg)]))]))

(define (type=? t1 t2)
  (match* (t1 t2)
    [('Nat 'Nat) #t]
    [(`(→ ,A1 ,B1) `(→ ,A2 ,B2))
     (and (type=? A1 A2) (type=? B1 B2))]
    [(_ _) #f]))
(define (type? t)
  (type=? t t))

;;; bidirectional typing means rule of judgement e : t be split to two forms e ⇒ t(synthesize type t from e) and e ⇐ t(check e has type t)
; these rules produce `synth` and `check`
(define (synth Γ exp)
  (match exp
    [`(the ,t ,e2)
     (if (not (type? t))
         (stop exp (format "Invalid type ~a" t))
         (go-on ([_ (check Γ e2 t)])
                (go t)))]
    [`(rec ,type ,target ,base ,step)
     (go-on ([target-t (synth Γ target)]
             [_ (if (type=? target-t 'Nat)
                    (go 'ok)
                    (stop target (format "Expected Nat, got ~v" target-t)))]
             [_ (check Γ base type)]
             [_ (check Γ step `(→ Nat (→ ,type ,type)))])
            (go type))]
    [x #:when (and (symbol? x)
                  (not (memv x '(the rec λ zero add1))))
       (match (assv x Γ)
         [#f (stop x "Variable not found")]
         [(cons _ t) (go t)])]
    [`(,rator ,rand)
     (go-on ([rator-t (synth Γ rator)])
            (match rator-t
              [`(→ ,A ,B)
               (go-on ([_ (check Γ rand A)])
                (go B))]
              [else (stop rator (format "Not a function type: ~v" rator-t))]))]))
(define (check Γ e t)
  (match e
    ['zero (if (type=? t 'Nat)
               (go 'ok)
               (stop e (format "Tried to use ~v for zero" t)))]
    [`(add1 ,n)
     (if (type=? t 'Nat)
         (go-on ([_ (check Γ n 'Nat)])
                (go 'ok))
         (stop e (format "Tried to use ~v for add1" t)))]
    [`(λ (,x) ,b)
     (match t
       [`(→ ,A ,B) (go-on ([_ (check (extend Γ x A) b B)])
                    (go 'ok))]
       [non-arrow (stop e (format "Instead of → type, get ~a" non-arrow))])]
    [other
     (go-on ([t2 (synth Γ e)])
            (if (type=? t t2)
                (go 'ok)
                (stop e
                      (format "Synthesized type ~v where type ~v was expected"
                              t2 t))))]))

(define (check-program Γ prog)
  (match prog
    ['() (go Γ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([t (synth Γ e)])
            (check-program (extend Γ x t) rest))]
    [(cons e rest)
     (go-on ([t (synth Γ e)])
            (begin
              (printf "~a has type ~a\n" e t)
              (check-program Γ rest)))]))

(struct ZERO () #:transparent)
(struct ADD1 (pred) #:transparent)
(struct NEU (type neu) #:transparent)
(struct THE (type value) #:transparent)
(define (norm? v)
  (THE? v))

(struct def (type value) #:transparent)

(define (defs->ctx Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def type _)) rest)
     (extend (defs->ctx rest) x type)]))
(define (defs->env Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def _ value)) rest)
     (extend (defs->env rest) x value)]))
