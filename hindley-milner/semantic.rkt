#lang typed/racket

(provide type/infer)

(require "lang.rkt"
         "typ.rkt"
         "pretty-print.rkt")

(struct Env
  [(parent : (Option Env))
   (type-env : (Mutable-HashTable String typ))]
  #:transparent
  #:mutable)
(: Env/new (->* () ((Option Env)) Env))
(define (Env/new [parent #f])
  (Env parent (make-hash '())))
;;; Env/lookup take variable name such as `x` to get a type from env
(: Env/lookup (-> Env String typ))
(define (Env/lookup env var-name)
  (: lookup-parent (-> typ))
  (define lookup-parent (λ ()
                          (: parent (Option Env))
                          (define parent (Env-parent env))
                          (if parent
                              ; dispatch to parent if we have one
                              (Env/lookup parent var-name)
                              ; really fail if we have no parent environment
                              (raise (format "no variable named: `~a`" var-name)))))
  ; try to get value from table
  (let ([typ-env : (Mutable-HashTable String typ) (Env-type-env env)])
    (hash-ref typ-env var-name
              ; if fail, handler would take
              lookup-parent)))
;;; Env/bind-var records form `x : A` into env, when lookup the variable `x` should get type `A`
(: Env/bind-var (-> Env String typ Void))
(define (Env/bind-var env var-name typ)
  (let ([env (Env-type-env env)])
    (if (hash-has-key? env var-name)
        (raise (format "redefined: `~a`" var-name))
        (hash-set! env var-name typ))))

(struct Context
  [(freevar-counter : Integer)
   (type-env : Env)]
  #:transparent
  #:mutable)
(: Context/new (-> Context))
(define (Context/new)
  (Context 0 (Env/new)))
(: Context/new-freevar! (-> Context typ))
(define (Context/new-freevar! ctx)
  (let ([cur-count (Context-freevar-counter ctx)])
    (set-Context-freevar-counter! ctx (+ 1 (Context-freevar-counter ctx)))
    (typ:freevar cur-count #f)))

(: occurs (-> typ typ Boolean))
(define (occurs v t)
  (match (cons v t)
    ([cons v (typ:freevar _ _)] (eqv? v t))
    ([cons v (typ:arrow t1 t2)] (or (occurs v t1) (occurs v t2)))
    ([cons v (typ:constructor _ type-params)]
     (foldl (λ ([t : typ] [pre-bool : Boolean])
              (or pre-bool (occurs v t)))
            #f
            type-params))
    (_ false)))

(: unify (-> typ typ Void))
(define (unify t1 t2)
  (match (cons t1 t2)
    ([cons (typ:constructor a al) (typ:constructor b bl)]
     #:when (string=? a b)
     (for-each (λ ((ae : typ) (be : typ))
                 (unify ae be))
               al bl))
    ([cons (typ:arrow p1 r1) (typ:arrow p2 r2)]
     (unify p1 p2)
     (unify r1 r2))
    ;;; freevar type is only important thing in `unify` function
    ((and
      [cons _ (typ:freevar _ _)]
      [cons t v])
     (if (or (eqv? v t) (not (occurs v t)))
         (subst! v t)
         (void))
     (void))
    ([cons (typ:freevar _ _) t2] (unify t2 t1))
    (_ (raise (format "cannot unify type ~a and ~a" (pretty-print t1) (pretty-print t2))))))

(: type/infer (->* (expr) (Context) typ))
(define (type/infer exp [ctx (Context/new)])
  (match exp
    ;;; some expressiones are easy to guess what it is
    ; in our **langauge** is `int`, `bool` and `string`
    ([expr:int _] (typ:builtin "int"))
    ([expr:bool _] (typ:builtin "bool"))
    ([expr:string _] (typ:builtin "string"))
    ;;; however there are some type is hard to guess, for example, `list`
    ; list type takes a type parameter to construct a concerete type
    ; we called it **type constructor**, we say `list a` is a type if `a` is a type
    ; however, we have no idea what is `a`, until any application create a type
    ; and we still have to store it, to express this concept, **type free variable** was introduced
    ; It could be a thing like `list ?0`, and `?0` should be replaced with a type later
    ; once `?0` bind to a type, we never change it
    ; the tricky part in inference type of list is: depends on list has elements or not, freevar added or not needed.
    ([expr:list elems]
     (typ:constructor "list"
                      (list (if (empty? elems)
                                (Context/new-freevar! ctx)
                                ; use first element type as type of all elements
                                (type/infer (car elems))))))
    ;;; infer variable would rely on lookup in context
    ([expr:variable name] (Env/lookup (Context-type-env ctx) name))
    ;;; infer lambda can be fun
    ; we will not just infer, but also affect the environment by binding
    ; lambda would need a new kind of type constructor, called arrow type, write as `A -> B`
    ; we say `A -> B` is a type if `A` and `B` both is a type.
    ([expr:lambda params body]
     ; params use new freevars as their type
     (let ([param-types (typ:constructor
                         "pair"
                         (map (λ ([param-name : String])
                                (let ([r (Context/new-freevar! ctx)])
                                  (Env/bind-var (Context-type-env ctx) param-name r)
                                  r)) params))]
           [body-typ (type/infer body ctx)])
       (typ:arrow param-types body-typ)))
    ;;; next we infer `let` binding
    ; people familiar with Racket might though: let binding is a lambda immediately be called
    ; in Racket, which make sense, in HM inference algorithm would make trouble
    ; the problem is **polymorphism lambda calculus(also known as System F)** is undecidable.
    ; only values bound in let-polymorphism construct are subject to instantiation.
    ([expr:let bindings exp]
     (: bind-to-context (-> (Pairof String expr) Void))
     (define bind-to-context (λ (bind)
                               (match bind
                                 ([cons name init]
                                  (Env/bind-var (Context-type-env ctx) name (type/infer init ctx))))))
     (map bind-to-context bindings)
     (type/infer exp ctx))
    ;;; the final expression is application
    ; infer application would require a new function called `unify`,
    ; which unified freevar and concrete type to give freevar a binding
    ([expr:application fn args]
     (let ([fn-typ (type/infer fn ctx)]
           [args-typ (map (λ ((arg : expr)) (type/infer arg ctx)) args)]
           [fresh (Context/new-freevar! ctx)])
       (unify fn-typ (typ:arrow (typ:constructor "pair" args-typ) fresh))
       fresh))))

(module+ test
  (require typed/rackunit))
(module+ test
  (test-case
   "infer type of simple expression"
   (check-equal? (type/infer (expr:int 1))
                 (typ:builtin "int"))
   (check-equal? (type/infer (expr:bool #f))
                 (typ:builtin "bool"))
   (check-equal? (type/infer (expr:string "know"))
                 (typ:builtin "string")))

  (test-case
   "list is a little bit free"
   (check-equal? (type/infer (expr:list '()))
                 (typ:constructor "list" (list (typ:freevar 0 #f)))))

  (test-case
   "let id function"
   (define exp (expr:let
                (list
                 (cons "id" (expr:lambda (list "x") (expr:variable "x"))))
                (expr:variable "id")))
   (check-equal? (type/infer exp)
                 ; expect: `(?0) -> ?0`
                 (typ:arrow (typ:constructor "pair" (list (typ:freevar 0 #f))) (typ:freevar 0 #f))))

  (test-case
   "let id function and apply"
   (define exp-app (expr:let
                    (list
                     (cons "id" (expr:lambda (list "x") (expr:variable "x"))))
                    (expr:application (expr:variable "id") (list (expr:bool #t)))))
   (check-equal? (type/infer exp-app)
                 (typ:freevar 1 (typ:builtin "bool"))))

  )
