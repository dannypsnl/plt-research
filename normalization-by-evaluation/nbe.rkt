#lang racket
;;; NOTE: totally from http://davidchristiansen.dk/tutorials/nbe/
(require (for-syntax syntax/parse))

;;; closure
(struct CLOS
  (env var body)
  #:transparent)

(define (extend env x v)
  (cons (cons x v) env))

(define (val env exp)
  (match exp
    [`(λ (,x) ,b)
     (CLOS env x b)]
    [x #:when (symbol? x)
       (let ([xv (assv x env)])
         (if xv
             (cdr xv)
             (error 'val "unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-application
      (val env rator)
      (val env rand))]))

(define (do-application fun arg)
  (match fun
    [(CLOS env x b)
     (val (extend env x arg) b)]
    [neutral-fun
     (N-application fun arg)]))

(define (run-program env exprs)
  (match exprs
    [(list) (void)]
    [(list `(define ,x ,e) rest ...)
     (let ([v (val env e)])
       (run-program (extend env x v) rest))]
    [(list e rest ...)
     (displayln (norm env e))
     (run-program env rest)]))

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
(struct N-var (name))
;;; neutral application
(struct N-application (rator rand))

(define (read-back used-names v)
  (match v
    [(CLOS env x body)
     (let* ([y (freshen used-names x)]
            [neutral-y (N-var y)])
       `(λ (,y)
          ,(read-back (cons y used-names)
                      (val (extend env x neutral-y) body))))]
    [(N-var x) x]
    [(N-application rator rand)
     `(,(read-back used-names rator)
       ,(read-back used-names rand))]))

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
(define (synth context exp)
  (match exp
    [`(the ,t ,e2)
     (if (not (type? t))
         (stop exp (format "Invalid type ~a" t))
         (go-on ([_ (check context e2 t)])
                (go t)))]
    [`(rec ,type ,target ,base ,step)
     (go-on ([target-t (synth context target)]
             [_ (if (type=? target-t 'Nat)
                    (go 'ok)
                    (stop target (format "Expected Nat, got ~v" target-t)))]
             [_ (check context base type)]
             [_ (check context step `(→ Nat (→ ,type ,type)))])
            (go type))]
    [x #:when (and (symbol? x)
                  (not (memv x '(the rec λ zero add1))))
       (match (assv x context)
         [#f (stop x "Variable not found")]
         [(cons _ t) (go t)])]
    [`(,rator ,rand)
     (go-on ([rator-t (synth context rator)])
            (match rator-t
              [`(→ ,A ,B)
               (go-on ([_ (check context rand A)])
                (go B))]
              [else (stop rator (format "Not a function type: ~v" rator-t))]))]))
(define (check context e t)
  (match e
    ['zero (if (type=? t 'Nat)
               (go 'ok)
               (stop e (format "Tried to use ~v for zero" t)))]
    [`(add1 ,n)
     (if (type=? t 'Nat)
         (go-on ([_ (check context n 'Nat)])
                (go 'ok))
         (stop e (format "Tried to use ~v for add1" t)))]
    [`(λ (,x) ,b)
     (match t
       [`(→ ,A ,B) (go-on ([_ (check (extend context x A) b B)])
                    (go 'ok))]
       [non-arrow (stop e (format "Instead of → type, get ~a" non-arrow))])]
    [other
     (go-on ([t2 (synth context e)])
            (if (type=? t t2)
                (go 'ok)
                (stop e
                      (format "Synthesized type ~v where type ~v was expected"
                              t2 t))))]))
