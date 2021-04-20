#lang typed/racket

(require "data.rkt")

(data expr
      [Var (name : String)]
      [abs (name : String) (body : expr)]
      [app (fn : expr) (arg : expr)]
      [pi (name : String) (e : expr) (body : expr)]
      [type (level : Integer)]
      [nat]
      [zero]
      [succ (n : expr)]
      [rec (e1 : expr) (e2 : expr) (e3 : expr) (e4 : expr)]
      [id (e1 : expr) (e2 : expr) (e3 : expr)]
      [refl (e : expr)]
      [J (e1 : expr) (e2 : expr) (e3 : expr) (e4 : expr) (e5 : expr) (e6 : expr)])

(data value
      [vabs (lam : (-> value value))]
      [vpi (v : value) (lam : (-> value value))]
      [vtype (level : Integer)]
      [vnat]
      [vzero]
      [vsucc (n : value)]
      [vid (v1 : value) (v2 : value) (v3 : value)]
      [vrefl (v : value)]
      [vneutral (n : neutral)])

(data neutral
      [nvar (name : String)]
      [napp (fn : neutral) (arg : value)]
      [nrec (n : neutral) (v1 : value) (v2 : value) (v3 : value)]
      [nj (v1 : value) (v2 : value) (v3 : value) (v4 : value) (v5 : value) (n : neutral)])

(: vapp : value value -> value)
(define (vapp u v)
  (match u
    [(vabs lam) (lam v)]
    [(vneutral n) (vneutral (napp n v))]
    [_ (error 'fail "u: ~a, v: ~a" u v)]))

(: eval : (Immutable-HashTable String value) expr -> value)
(define (eval env t)
  (match t
    [(Var name) (hash-ref env name
                          (λ () (vneutral (nvar name))))]
    [(abs x e) (vabs (λ (v) (eval (hash-set env x v) e)))]
    [(app fn arg) (vapp (eval env fn) (eval env arg))]
    [(pi x a e) (vpi (eval env a) (λ (v) (eval (hash-set env x v) e)))]
    [(type i) (vtype i)]
    [(nat) (vnat)]
    [(zero) (vzero)]
    [(succ e) (vsucc (eval env e))]))
