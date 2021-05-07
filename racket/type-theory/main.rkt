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

(define-type Env (Immutable-HashTable Symbol value))
(define-type TypeEnv (Immutable-HashTable Symbol value))

(: vapp : value value -> value)
(define (vapp u v)
  (match u
    [(vabs lam) (lam v)]
    [(vneutral n) (vneutral (napp n v))]
    [_ (error 'fail "u: ~a, v: ~a" u v)]))

(: var : Symbol -> value)
(define (var s)
  (vneutral (nvar s)))

(: eval : TypeEnv expr -> value)
(define (eval env t)
  (match t
    [(Var name) (hash-ref env name
                          (λ () (var name)))]
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
     (abs x (readback (f (var x)))))]
  [((vpi a b))
   (let ([x (gensym)])
     (pi x (readback a) (readback (b (var x)))))]
  [((vtype i)) (type i)]
  [((vnat)) (nat)]
  [((vzero)) (zero)]
  [((vsucc n)) (succ (readback n))]
  [((vid a t u)) (id (readback a) (readback t) (readback u))]
  [((vrefl t)) (refl (readback t))]
  [((vneutral t)) (readback-neutral t)])

(: veq : value value -> Boolean)
(define (veq t u)
  (match* {t u}
    [{(vpi a _) (vpi b _)} (equal? (readback a) (readback b))]
    [{a b} (equal? (readback a) (readback b))]))

(: infer : TypeEnv Env expr -> value)
(define (infer tenv env e)
  (match e
    [(Var x) (hash-ref tenv x
                       (λ () (error 'unbound-variable "~a" x)))]
    [(app t u)
     (match (infer tenv env t)
       [(vpi a b)
        (check tenv env u a)
        (b (eval env u))]
       [_ (error 'not-appliable)])]
    [(pi x a b)
     (let* ([i (universe tenv env a)]
            [a (eval env a)]
            [j (universe (hash-set tenv x a) env b)])
       (vtype (max i j)))]
    [(type i) (vtype (add1 i))]
    [(nat) (vtype 0)]
    [(zero) (vnat)]
    [(succ t)
     (check tenv env t (vnat))
     (vnat)]
    [(rec n a z s)
     (check tenv env n (vnat))
     (match (eval env a)
       [(vpi (vnat) a)
        (let ([n (eval env n)])
          (check tenv env z (a (vzero)))
          (check tenv env s
                 (vpi (vnat) (λ (n) (varr (a n) (a (vsucc n))))))
          (a n))]
       [_ (error 'type-error)])]
    [(id a t u)
     (let ([i (universe tenv env a)]
           [a (eval env a)])
       (check tenv env t a)
       (check tenv env u a)
       (vtype i))]
    [(refl t)
     (let ([a (infer tenv env t)]
           [t (eval env t)])
       (vid a t t))]
    [(J a p r t u e)
     (let ([i (universe tenv env a)]
           [a (eval env a)])
       (check tenv env p
              (vpi a (λ (x)
                       (vpi a (λ (y)
                                (varr (vid a x y) (vtype i)))))))
       (let ([p1 (eval env p)])
         (: p : value value value -> value)
         (define (p x y e) (vapp (vapp (vapp p1 x) y) e))
         (check tenv env r (vpi a (λ (x) (p x x (vrefl x)))))
         (check tenv env t a)
         (check tenv env u a)
         (let ([t (eval env t)]
               [u (eval env u)])
           (check tenv env e (vid a t u))
           (p t u (eval env e)))))]
    [(abs _ _) (error 'type-error)]))

(: varr : value value -> value)
(define (varr a b)
  (vpi a (λ (_) b)))

(: universe : TypeEnv Env expr -> Integer)
(define (universe tenv env t)
  (match (infer tenv env t)
    [(vtype i) i]
    [else (error 'type-error)]))

(: check : TypeEnv Env expr value -> Void)
(define (check tenv env t a)
  (match* {t a}
    [{(abs x t) (vpi a b)}
     (let ([y (var (gensym 'k))])
       (check (hash-set tenv x a)
              (hash-set env x y)
              t
              (b y)))]
    [{(refl t) (vid _ u v)}
     (let ([t (eval env t)])
       (unless (and (veq t u)
                    (veq t v))
         (error 'type-error)))]
    [{t a}
     (unless (veq a (infer tenv env t))
       (error 'type-error
              "~a ~a"
              a (infer tenv env t)))]))

(module+ test
  (let ([a (varr (vnat) (varr (vnat) (vnat)))]
        [t (abs 'm
                (rec (Var 'm)
                  (pi '_ (nat) (pi '_ (nat) (nat)))
                  (abs 'n (Var 'n))
                  (abs 'm
                       (abs 'r
                            (abs 'n (succ (app (Var 'r) (Var 'n))))))))])
    (check (make-immutable-hash)
           (make-immutable-hash)
           t a)))
