#lang typed/racket

(require "data.rkt")

(data expr
      [Var (name : Symbol)]
      [abs (name : Symbol) (body : expr)]
      [app (fn : expr) (arg : expr)]
      [pi (name : Symbol) (e : expr) (body : expr)]
      [type (level : Integer)]
      [nat]
      [zero]
      [succ (n : expr)]
      [rec expr expr expr expr]
      [id expr expr expr]
      [refl expr]
      [J expr expr expr expr expr expr])

(data value
      [vabs (lam : (-> value value))]
      [vpi (v : value) (lam : (-> value value))]
      [vtype (level : Integer)]
      [vnat]
      [vzero]
      [vsucc (n : value)]
      [vid value value value]
      [vrefl value]
      [vneutral (n : neutral)])

(data neutral
      [nvar (name : Symbol)]
      [napp (fn : neutral) (arg : value)]
      [nrec (n : neutral) value value value]
      [nj value value value value value (n : neutral)])

(: vapp : value value -> value)
(define (vapp u v)
  (match u
    [(vabs lam) (lam v)]
    [(vneutral n) (vneutral (napp n v))]
    [_ (error 'fail "u: ~a, v: ~a" u v)]))

(: eval : (Immutable-HashTable Symbol value) expr -> value)
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
    [(succ e) (vsucc (eval env e))]
    [(rec n a z s)
     (let ([n (eval env n)]
           [a (eval env a)]
           [z (eval env z)]
           [s (eval env s)])
       (letrec ([f : (-> value value)
                   (match-lambda
                     [(vzero) z]
                     [(vsucc n) (vapp (vapp s n) (f n))]
                     [(vneutral n) (vneutral (nrec n a z s))]
                     [_ (error 'fail "rec")])])
         (f n)))]
    [(id a t u) (vid (eval env a) (eval env t) (eval env u))]
    [(refl t) (vrefl (eval env t))]
    [(J a p r t u e)
     (match (eval env e)
       [(vrefl _) (eval env r)]
       [(vneutral e)
        (vneutral (nj (eval env a) (eval env p) (eval env r) (eval env t) (eval env u) e))]
       [_ (error 'fail "t: ~a" t)])]))

(: readback-neutral : neutral -> expr)
(define/match (readback-neutral n)
  [((nvar x)) (Var x)]
  [((napp t u))
   (app (readback-neutral t) (readback u))]
  [((nrec n a z s))
   (rec (readback-neutral n) (readback a) (readback z) (readback s))]
  [((nj a p r t u e))
   (J (readback a) (readback p) (readback r)
      (readback t) (readback u) (readback-neutral e))])
(: readback : value -> expr)
(define/match (readback v)
  [((vabs f))
   (let ([x (gensym 'k)])
     (abs x (readback (f (vneutral (nvar x))))))]
  [((vpi a b))
   (let ([x (gensym)])
     (pi x (readback a) (readback (b (vneutral (nvar x))))))]
  [((vtype i)) (type i)]
  [((vnat)) (nat)]
  [((vzero)) (zero)]
  [((vsucc n)) (succ (readback n))]
  [((vid a t u)) (id (readback a) (readback t) (readback u))]
  [((vrefl t)) (refl (readback t))]
  [((vneutral t)) (readback-neutral t)])

(: veq : value value -> Boolean)
(define (veq t u)
  (equal? (readback t) (readback u)))
