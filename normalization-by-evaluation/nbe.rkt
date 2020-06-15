#lang racket
;;; NOTE: totally from http://davidchristiansen.dk/tutorials/nbe/
(require (for-syntax syntax/parse))

(struct PI (domain range) #:transparent)
(struct LAM (body) #:transparent)
(struct SIGMA (car-type cdr-type) #:transparent)
(struct PAIR (car cdr) #:transparent)
(struct NAT () #:transparent)
(struct ZERO () #:transparent)
(struct ADD1 (pred) #:transparent)
(struct EQ (type from to) #:transparent)
(struct SAME () #:transparent)
(struct TRIVIAL () #:transparent)
(struct SOLE () #:transparent)
(struct ABSURD () #:transparent)
(struct ATOM () #:transparent)
(struct QUOTE (symbol) #:transparent)
(struct UNI () #:transparent)
(struct NEU (type neutral) #:transparent)
(struct H-O-CLOS (x fun) #:transparent)
(define (closure-name c)
  (match c
    [(CLOS _ x _) x]
    [(H-O-CLOS x _) x]))
(struct THE (type value) #:transparent)
;;; closure
(struct CLOS
  (env var body)
  #:transparent)

(define (extend env x typ)
  (cons (cons x typ) env))

(define (val ρ e)
  (match e
    [`(the ,type ,expr)
     (val ρ expr)]
    ['U (UNI)]
    [`(Π ((,x ,A)) ,B)
     (PI (val ρ A) (CLOS ρ x B))]
    [`(λ (,x) ,b)
     (LAM (CLOS ρ x b))]
    [`(Σ ((,x ,A)) ,D)
     (SIGMA (val ρ A) (CLOS ρ x D))]
    [`(cons ,a ,d)
     (PAIR (val ρ a) (val ρ d))]
    [`(car ,pr)
     (do-car (val ρ pr))]
    [`(cdr ,pr)
     (do-cdr (val ρ pr))]
    ['Nat (NAT)]
    ['zero (ZERO)]
    [`(add1 ,n) (ADD1 (val ρ n))]
    [`(ind-Nat ,target ,motive ,base ,step)
     (do-ind-Nat (val ρ target) (val ρ motive) (val ρ base) (val ρ step))]
    [`(= ,A ,from ,to)
     (EQ (val ρ A) (val ρ from) (val ρ to))]
    ['same
     (SAME)]
    [`(replace ,target ,motive ,base)
     (do-replace (val ρ target) (val ρ motive) (val ρ base))]
    ['Trivial (TRIVIAL)]
    ['sole (SOLE)]
    ['Absurd (ABSURD)]
    [`(ind-Absurd ,target ,motive) (do-ind-Absurd (val ρ target) (val ρ motive))]
    ['Atom (ATOM)]
    [`',a  (QUOTE a)]
    [`(,rator ,rand)
     (do-application (val ρ rator) (val ρ rand))]
    [x #:when (var? x)
       (cdr (assv x ρ))]))

(define (do-car v)
  (match v
    [(PAIR a d) a]
    [(NEU (SIGMA A _) ne)
     (NEU A (N-car ne))]))
(define (do-cdr v)
  (match v
    [(PAIR a d) d]
    [(NEU (SIGMA _ D) ne)
     (NEU (val-of-closure D (do-car v))
          (N-cdr ne))]))
(define (do-application fun arg)
  (match fun
    [(LAM c)
     (val-of-closure c arg)]
    [(NEU (PI A B) ne)
     (NEU (val-of-closure B arg)
          (N-application ne (THE A arg)))]))
(define (do-ind-Absurd target motive)
  (match target
    [(NEU (ABSURD) ne)
     (NEU motive (N-ind-Absurd ne (THE (UNI) motive)))]))
(define (do-replace target motive base)
  (match target
    [(SAME) base]
    [(NEU (EQ A from to) ne)
     (NEU (do-application motive to)
          (N-replace ne
                     (THE (PI A (H-O-CLOS 'x (lambda (_) (UNI))))
                          motive)
                     (THE (do-application motive from)
                          base)))]))
(define (do-ind-Nat target motive base step)
  (match target
    [(ZERO) base]
    [(ADD1 n) (do-application (do-application step n) (do-ind-Nat n motive base step))]
    [(NEU (NAT) ne)
     (NEU (do-application motive target)
          (N-ind-Nat
           ne
           (THE (PI (NAT)
                    (H-O-CLOS 'k (lambda (k) (UNI))))
                motive)
           (THE (do-application motive (ZERO)) base)
           (THE (ind-Nat-step-type motive)
                step)))]))
(define (ind-Nat-step-type motive)
  (PI (NAT)
      (H-O-CLOS 'n-1
                (lambda (n-1)
                  (PI (do-application motive n-1)
                      (H-O-CLOS 'ih
                                (lambda (ih)
                                  (do-application motive (ADD1 n-1)))))))))

(define (run-program Γ inputs)
  (match inputs
    ['() (go Γ)]
    [(cons d rest)
     (go-on ([new-Γ (interact Γ d)])
            (run-program new-Γ rest))]))

(define (freshen used x)
  (define (add-* x) (string->symbol (string-append (symbol->string x) "*")))
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
(struct N-application (rator rand) #:transparent)
(struct N-car (pair) #:transparent)
(struct N-cdr (pair) #:transparent)
(struct N-ind-Nat (target motive base step) #:transparent)
(struct N-replace (target motive base) #:transparent)
(struct N-ind-Absurd (target motive) #:transparent)

(define (read-back-norm Γ norm)
  (match norm
    [(THE (NAT) (ZERO)) 'zero]
    [(THE (NAT) (ADD1 n))
     `(add1 ,(read-back-norm Γ (THE (NAT) n)))]
    [(THE (PI A B) f)
     (define x (closure-name B))
     (define y (freshen (map car Γ) x))
     (define y-val (NEU A (N-var y)))
     `(λ (,y)
        ,(read-back-norm (extend-ctx Γ y A)
                         (THE (val-of-closure B y-val)
                              (do-application f y-val))))]
    [(THE (SIGMA A D) p)
     (define the-car (THE A (do-car p)))
     (define the-cdr (THE (val-of-closure D the-car) (do-cdr p)))
     `(cons ,(read-back-norm Γ the-car) ,(read-back-norm Γ the-cdr))]
    [(THE (TRIVIAL) _) 'sole]
    [(THE (ABSURD) (NEU (ABSURD) ne))
     `(the Absurd
           ,(read-back-neutral Γ ne))]
    [(THE (EQ A from to) (SAME)) 'same]
    [(THE (ATOM) (QUOTE x)) `',x]
    [(THE (UNI) (NAT)) 'Nat]
    [(THE (UNI) (ATOM)) 'Atom]
    [(THE (UNI) (TRIVIAL)) 'Trivial]
    [(THE (UNI) (ABSURD)) 'Absurd]
    [(THE (UNI) (EQ A from to))
     `(= ,(read-back-norm Γ (THE (UNI) A))
         ,(read-back-norm Γ (THE A from))
         ,(read-back-norm Γ (THE A to)))]
    [(THE (UNI) (SIGMA A D))
     (define x (closure-name D))
     (define y (freshen (map car Γ) x))
     `(Σ ((,y ,(read-back-norm Γ (THE (UNI) A))))
         ,(read-back-norm (extend-ctx Γ y A)
                          (THE (UNI) (val-of-closure D (NEU A (N-var y))))))]
    [(THE (UNI) (PI A B))
     (define x (closure-name B))
     (define y (freshen (map car Γ) x))
     `(Π ((,y ,(read-back-norm Γ (THE (UNI) A))))
         ,(read-back-norm (extend-ctx Γ y A)
                          (THE (UNI) (val-of-closure B (NEU A (N-var y))))))]
    [(THE (UNI) (UNI)) 'U]
    [(THE t1 (NEU t2 ne))
     (read-back-neutral Γ ne)]))
(define (read-back-neutral Γ neu)
  (match neu
    [(N-var x) x]
    [(N-application ne rand)
     `(,(read-back-neutral Γ ne)
       ,(read-back-norm Γ rand))]
    [(N-car ne) `(car ,(read-back-neutral Γ ne))]
    [(N-cdr ne) `(cdr ,(read-back-neutral Γ ne))]
    [(N-ind-Nat ne motive base step)
     `(ind-Nat ,(read-back-neutral Γ ne)
               ,(read-back-norm Γ motive)
               ,(read-back-norm Γ base)
               ,(read-back-norm Γ step))]
    [(N-replace ne motive base)
     `(replace ,(read-back-neutral Γ ne)
               ,(read-back-norm Γ motive)
               ,(read-back-norm Γ base))]
    [(N-ind-Absurd ne motive)
     `(ind-Absurd (the Absurd ,(read-back-neutral Γ ne))
                  ,(read-back-norm Γ motive))]))

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
(define (synth Γ e)
  (match e
    [`(the ,type ,expr)
     (go-on ([t-out (check Γ type (UNI))]
             [e-out (check Γ expr (val (ctx->env Γ) t-out))])
       (go `(the ,t-out ,e-out)))]
    ['U
     (go '(the U U))]
    [`(,(or 'Σ 'Sigma) ((,x ,A)) ,D)
     (go-on ([A-out (check Γ A (UNI))]
             [D-out (check (extend-ctx Γ x (val (ctx->env Γ) A-out)) D (UNI))])
       (go `(the U (Σ ((,x ,A-out)) ,D-out))))]
    [`(car ,pr)
     (go-on ([`(the ,pr-ty ,pr-out) (synth Γ pr)])
       (match (val (ctx->env Γ) pr-ty)
         [(SIGMA A D)
          (go `(the ,(read-back-norm Γ (THE (UNI) A)) (car ,pr-out)))]
         [non-SIGMA
          (stop e (format "Expected Σ, got ~v"
                          (read-back-norm Γ (THE (UNI) non-SIGMA))))]))]
    [`(cdr ,pr)
     (go-on ([`(the ,pr-ty ,pr-out) (synth Γ pr)])
       (match (val (ctx->env Γ) pr-ty)
         [(SIGMA A D)
          (define the-car (do-car (val (ctx->env Γ) pr-out)))
          (go `(the ,(read-back-norm Γ (THE (UNI) (val-of-closure D the-car)))
                    (cdr ,pr-out)))]
         [non-SIGMA
          (stop e (format "Expected Σ, got ~v"
                          (read-back-norm Γ (THE (UNI) non-SIGMA))))]))]
    ['Nat (go '(the U Nat))]
    [`(ind-Nat ,target ,motive ,base ,step)
     (go-on ([target-out (check Γ target (NAT))]
             [motive-out (check Γ motive (PI (NAT) (H-O-CLOS 'n (lambda (_) (UNI)))))]
             [motive-val (go (val (ctx->env Γ) motive-out))]
             [base-out (check Γ base (do-application motive-val (ZERO)))]
             [step-out (check Γ
                              step
                              (ind-Nat-step-type motive-val))])
       (go `(the ,(read-back-norm
                   Γ
                   (THE (UNI)
                        (do-application motive-val (val (ctx->env Γ) target-out))))
                 (ind-Nat ,target-out ,motive-out ,base-out ,step-out))))]
    [`(= ,A ,from ,to)
     (go-on ([A-out (check Γ A (UNI))]
             [A-val (go (val (ctx->env Γ) A-out))]
             [from-out (check Γ from A-val)]
             [to-out (check Γ to A-val)])
       (go `(the U (= ,A-out ,from-out ,to-out))))]
    [`(replace ,target ,motive ,base)
     (go-on ([`(the ,target-t ,target-out) (synth Γ target)])
       (match (val (ctx->env Γ) target-t)
         [(EQ A from to)
          (go-on ([motive-out
                   (check Γ
                          motive
                          (PI A (H-O-CLOS 'x (lambda (x) (UNI)))))]
                  [motive-v (go (val (ctx->env Γ) motive-out))]
                  [base-out (check Γ base (do-application motive-v from))])
            (go `(the ,(read-back-norm Γ (THE (UNI) (do-application motive-v to)))
                      (replace ,target-out ,motive-out ,base-out))))]
         [non-EQ
          (stop target (format "Expected =, but type is ~a" non-EQ))]))]
    [`(,(or 'Π 'Pi) ((,x ,A)) ,B)
     (go-on ([A-out (check Γ A (UNI))]
             [B-out (check (extend-ctx Γ x (val (ctx->env Γ) A-out)) B (UNI))])
       (go `(the U (Π ((,x ,A-out)) ,B-out))))]
    ['Trivial (go '(the U Trivial))]
    ['Absurd (go '(the U Absurd))]
    [`(ind-Absurd ,target ,motive)
     (go-on ([target-out (check Γ target (ABSURD))]
             [motive-out (check Γ motive (UNI))])
       (go `(the ,motive-out (ind-Absurd ,target-out ,motive-out))))]
    ['Atom (go '(the U Atom))]
    [`(,rator ,rand)
     (go-on ([`(the ,rator-t ,rator-out) (synth Γ rator)])
       (match (val (ctx->env Γ) rator-t)
         [(PI A B)
          (go-on ([rand-out (check Γ rand A)])
            (go `(the ,(read-back-norm Γ
                                       (THE (UNI)
                                            (val-of-closure B
                                                            (val (ctx->env Γ)
                                                                 rand-out))))
                      (,rator-out ,rand-out))))]
         [non-PI (stop rator
                       (format "Expected a Π type, but this is a ~a"
                               (read-back-norm Γ (THE (UNI) non-PI))))]))]
    [x #:when (var? x)
     (go-on ([t (lookup-type x Γ)])
       (go `(the ,(read-back-norm Γ (THE (UNI) t)) ,x)))]
    [none-of-the-above (stop e "Can't synthesize a type")]))
(define (check Γ e t)
  (match e
    [`(cons ,a ,d)
     (match t
       [(SIGMA A D)
        (go-on ([a-out (check Γ a A)]
                [d-out (check Γ d (val-of-closure D (val (ctx->env Γ) a-out)))])
          (go `(cons ,a-out ,d-out)))]
       [non-SIGMA (stop e (format "Expected Σ, got ~v"
                                  (read-back-norm Γ (THE (UNI) non-SIGMA))))])]
    ['zero
     (match t
       [(NAT) (go 'zero)]
       [non-NAT (stop e (format "Expected Nat, got ~v"
                                (read-back-norm Γ (THE (UNI) non-NAT))))])]
    [`(add1 ,n)
     (match t
       [(NAT)
        (go-on ([n-out (check Γ n (NAT))])
          (go `(add1 ,n-out)))]
       [non-NAT (stop e (format "Expected Nat, got ~v"
                                (read-back-norm Γ (THE (UNI) non-NAT))))])]
    ['same
     (match t
       [(EQ A from to)
        (go-on ([_ (convert Γ A from to)])
          (go 'same))]
       [non-= (stop e (format "Expected =, got ~v"
                              (read-back-norm Γ (THE (UNI) non-=))))])]
    ['sole
     (match t
       [(TRIVIAL)
        (go 'sole)]
       [non-Trivial (stop e (format "Expected Trivial, got ~v"
                                    (read-back-norm Γ (THE (UNI) non-Trivial))))])]
    [`(,(or 'λ 'lambda) (,x) ,b)
     (match t
       [(PI A B)
        (define x-val (NEU A (N-var x)))
        (go-on ([b-out (check (extend-ctx Γ x A) b (val-of-closure B x-val))])
          (go `(λ (,x) ,b-out)))]
       [non-PI (stop e (format "Expected Π, got ~v"
                               (read-back-norm Γ (THE (UNI) non-PI))))])]
    [`',a
     (match t
       [(ATOM)
        (go `',a)]
       [non-ATOM (stop e (format "Expected Atom, got ~v"
                                 (read-back-norm Γ (THE (UNI) non-ATOM))))])]
    [none-of-the-above
     (go-on ([`(the ,t-out ,e-out) (synth Γ e)]
             [_ (convert Γ (UNI) t (val (ctx->env Γ) t-out))])
       (go e-out))]))
(define (convert Γ t v1 v2)
  (define e1 (read-back-norm Γ (THE t v1)))
  (define e2 (read-back-norm Γ (THE t v2)))
  (if (α-equiv? e1 e2)
      (go 'ok)
      (stop e1 (format "Expected to be the same ~v as ~v"
                       (read-back-norm Γ (THE (UNI) t))
                       e2))))
(define (interact Γ input)
  (match input
    [`(define ,x ,e)
     (if (assv x Γ)
         (stop x "Already defined")
         (go-on ([`(the ,ty ,expr) (synth Γ e)])
                (let ([ρ (ctx->env Γ)])
                  (go (cons (cons x (def (val ρ ty) (val ρ expr)))
                            Γ)))))]
    [e
     (go-on ([`(the ,ty ,expr) (synth Γ e)])
            (let ([ρ (ctx->env Γ)])
              (begin
                (printf "Type: ~v\nNormal form:~v\n"
                        ty
                        (read-back-norm Γ
                                        (THE (val ρ ty)
                                             (val ρ expr))))
                (go Γ))))]))

(struct def (type value) #:transparent)
(struct bind (type) #:transparent)

(define (context? Γ)
  (match Γ
    ['() #t]
    [(cons (cons x b) rest)
     (and (symbol? x) (or (def? b) (bind? b)) (context? rest))]
    [_ #f]))
(define (lookup-type x Γ)
  (match (assv x Γ)
    [#f (stop x "Unknown variable")]
    [(cons _ (bind type)) (go type)]
    [(cons _ (def type _)) (go type)]))
(define (ctx->env Γ)
  (map (lambda (binder)
         (match binder
           [(cons name (bind type))
            (cons name
                  (NEU type
                       (N-var name)))]
           [(cons name (def _ value))
            (cons name value)]))
       Γ))
(define (extend-ctx Γ x t)
  (cons (cons x (bind t)) Γ))
(define (val-of-closure c v)
  (match c
    [(CLOS ρ x e) (val (extend ρ x v) e)]
    [(H-O-CLOS x f) (f v)]))

(define keywords
  '(define
     U
     Nat
     zero
     add1
     ind-Nat
     Σ Sigma
     cons car cdr
     Π Pi
     λ lambda
     = same replace
     Trivial sole
     Absurd ind-Absurd
     Atom quote
     the))

(define (keyword? k)
  (if (memv k keywords)
      #t
      #f))
(define (var? x) (and (symbol? x) (not (keyword? x))))

(define (α-equiv? e1 e2)
  (α-equiv-aux e1 e2 '() '()))
(define (α-equiv-aux e1 e2 xs1 xs2)
  (match* (e1 e2)
    [(kw kw) #:when (keyword? kw) #t]
    [(x y)
     ;;; both are variables
     #:when (and (var? x) (var? y))
     (match* ((assv x xs1) (assv y xs2))
       [(#f #f) (eqv? x y)]
       [((cons _ b1) (cons _ b2)) (eqv? b1 b2)]
       [(_ _) #f])]
    [(`(λ (,x) ,b1) `(λ (,y) ,b2))
     (let ([fresh (gensym)])
       ; λ introduces free var
       (let ([bigger1 (cons (cons x fresh) xs1)]
             [bigger2 (cons (cons y fresh) xs2)])
         (α-equiv-aux b1 b2 bigger1 bigger2)))]
    [(`(Π ((,x ,A1)) ,B1) `(Π ((,y ,A2)) ,B2))
     (and (α-equiv-aux A1 A2 xs1 xs2)
          ; Π introduces free type
          (let ([fresh (gensym)])
            (let ([bigger1 (cons (cons x fresh) xs1)]
                  [bigger2 (cons (cons y fresh) xs2)])
              (α-equiv-aux B1 B2 bigger1 bigger2))))]
    [(`(Σ ((,x ,A1)) ,B1) `(Σ ((,y ,A2)) ,B2))
     (and (α-equiv-aux A1 A2 xs1 xs2)
          ; Σ introduces free type
          (let ([fresh (gensym)])
            (let ([bigger1 (cons (cons x fresh) xs1)]
                  [bigger2 (cons (cons y fresh) xs2)])
              (α-equiv-aux B1 B2 bigger1 bigger2))))]
    [(`',x `',y)
     (eqv? x y)]
    ; This, together with read-back-norm, implements the η law for Absurd.
    [(`(the Absurd ,e1) `(the Absurd e2))
     #t]
    [((cons op args1) (cons op args2))
     #:when (keyword? op)
     (and (= (length args1) (length args2))
          (for/and ([arg1 (in-list args1)]
                    [arg2 (in-list args2)])
            (α-equiv-aux arg1 arg2 xs1 xs2)))]
    [((list rator1 rand1) (list rator2 rand2))
     (and (α-equiv-aux rator1 rator2 xs1 xs2)
          (α-equiv-aux rand1 rand2 xs1 xs2))]
    [(_ _) #f]))

;;; example
(void
   (run-program '()
                '(; What are the consequences of Nat equality?
                  (define nat=consequence
                    (the (Pi ((j Nat))
                           (Pi ((k Nat))
                             U))
                         (lambda (j)
                           (lambda (k)
                             (ind-Nat j
                                      (lambda (_) U)
                                      (ind-Nat k
                                               (lambda (_) U)
                                               Trivial
                                               (lambda (_)
                                                 (lambda (_)
                                                   Absurd)))
                                      (lambda (j-1)
                                        (lambda (_)
                                          (ind-Nat k
                                                   (lambda (_) U)
                                                   Absurd
                                                   (lambda (k-1)
                                                     (lambda (_)
                                                       (= Nat j-1 k-1)))))))))))
                  ; The consequences hold for Nats that are the same
                  (define nat=consequence-refl
                    (the (Pi ((n Nat))
                           ((nat=consequence n) n))
                         (lambda (n)
                           (ind-Nat n
                                    (lambda (k)
                                      ((nat=consequence k) k))
                                    sole
                                    (lambda (n-1)
                                      (lambda (_)
                                        same))))))
                  (nat=consequence-refl zero)
                  (nat=consequence-refl (add1 (add1 zero)))
                  ; The consequences hold for all equal Nats
                  (define there-are-consequences
                    (the (Pi ((j Nat))
                           (Pi ((k Nat))
                             (Pi ((j=k (= Nat j k)))
                               ((nat=consequence j) k))))
                         (lambda (j)
                           (lambda (k)
                             (lambda (j=k)
                               (replace j=k
                                        (lambda (n)
                                          ((nat=consequence j) n))
                                        (nat=consequence-refl j)))))))
                  ((there-are-consequences zero) zero)
                  (((there-are-consequences zero) zero) same)
                  ((there-are-consequences (add1 zero)) (add1 zero))
                  (((there-are-consequences (add1 zero)) (add1 zero)) same)
                  ((there-are-consequences zero) (add1 zero))
                  ((there-are-consequences (add1 zero)) zero))))
