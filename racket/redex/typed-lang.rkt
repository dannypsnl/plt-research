#lang racket
(require redex
         redex/tut-subst
         pict)

(define-language L
  (e (e e)
     (λ (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ t t)
     num)
  (x variable-not-otherwise-mentioned))

(define-extended-language L+Γ L
  [Γ • (x : t Γ)])

(define-metafunction L+Γ
  [(different? x_1 x_1) #f]
  [(different? x_1 x_2) #t])
(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e t)

  [(types Γ e_1 (→ t_2 t_3))
   (types Γ e_2 t_2)
   -------------------------
   (types Γ (e_1 e_2) t_3)]

  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]

  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   ---------------------------------------
   (types Γ (fix e) (→ t_1 t_2))]

  [---------------------
   (types (x : t Γ) x t)]

  [(types Γ x_1 t_1)
   (side-condition (different? x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]

  [(types Γ e num) ...
   -----------------------
   (types Γ (+ e ...) num)]

  [--------------------
   (types Γ number num)]

  [(types Γ e_1 num)
   (types Γ e_2 t)
   (types Γ e_3 t)
   -----------------------------
   (types Γ (if0 e_1 e_2 e_3) t)]

  [(types Γ e num) ...
   --------------------------
   (types Γ (amb e ...) num)])

(module+ test
  (test-equal
   (judgment-holds
    (types • (amb 1 2 3) t)
    t)
   (list (term num)))

  (test-equal
   (judgment-holds
    (types •
           ((λ (x num) (amb x 1))
            (+ 1 2))
           t)
    t)
   (list (term num)))

  (test-equal
   (judgment-holds
    (types •
           (λ (f (→ num (→ num num))) (f (amb 1 2)))
           (→ t_1 t_2))
    t_2)
   (list (term (→ num num)))))

(with-compound-rewriters
    (['different?
      (λ (lws)
        (list "" (list-ref lws 2) " ≠ " (list-ref lws 3) ""))]
     ['types
      (λ (lws)
        (list "" (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4) ""))])
  (scale (judgment-form->pict types)
         1.1))

(define-extended-language Ev L+Γ
  (p (e ...))
  (P (e ... E e ...))
  (E (v E)
     (E e)
     (+ v ... E e ...)
     (if0 E e e)
     (fix E)
     hole)
  (v (λ (x t) e)
     (fix v)
     number))

(define-metafunction Ev
  Σ : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])
(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match Ev x))

(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole P (if0 0 e_1 e_2))
        (in-hole P e_1)
        "if0t")
   (--> (in-hole P (if0 v e_1 e_2))
        (in-hole P e_2)
        (side-condition (not (equal? 0 (term v))))
        "if0f")
   (--> (in-hole P ((fix (λ (x t) e)) v))
        (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
        "fix")
   (--> (in-hole P ((λ (x t) e) v))
        (in-hole P (subst x v e))
        "βv")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...)
        (e_1 ... (in-hole E e_2) ... e_3 ...)
        "amb")))

#;(traces red
          (term ((+ (amb 1 2)
                    (amb 10 20)))))

(module+ test
  (test-->>
   red
   (term ((+ (amb 1 2)
             (amb 10 20))))
   (term (11 21 12 22)))

  (test-results))
