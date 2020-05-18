#lang typed/racket

(require "lang.rkt"
         "typ.rkt")

(struct env
  [(parent : (Option env))
   (type-env : (Mutable-HashTable String typ))]
  #:transparent
  #:mutable)
(: env/new (->* () ((Option env)) env))
(define (env/new [parent #f])
  (env parent (make-hash '())))
(: env/lookup (-> env String typ))
(define (env/lookup env var-name)
  (hash-ref (env-type-env env) var-name
            (λ ()
              (match env
                (e (env/lookup e var-name))
                (#f (raise (format "no variable named: `~a`" var-name)))))))
;;; env/bind-var should record a form `x : A` into env, when lookup the variable `x` should get type `A`
(: env/bind-var (-> env String typ Void))
(define (env/bind-var env var-name typ)
  (let ([env (env-type-env env)])
    (if (hash-has-key? env var-name)
        (raise (format "redefined: `~a`" var-name))
        (hash-set! env var-name typ))))

(struct Context
  [(freevar-counter : Integer)
   (type-env : env)]
  #:transparent
  #:mutable)
(: Context/new (-> Context))
(define (Context/new)
  (Context 0 (env/new)))
(: Context/freevar! (-> Context Integer))
(define (Context/freevar! ctx)
  (let ([cur-count (Context-freevar-counter ctx)])
    (set-Context-freevar-counter! ctx (+ 1 (Context-freevar-counter ctx)))
    cur-count))

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
                                (typ:freevar (Context/freevar! ctx))
                                ; use first element type as type of all elements
                                (type/infer (car elems))))))
    ;;; infer variable would rely on lookup in context
    ([expr:variable name] (env/lookup (Context-type-env ctx) name))
    (_ (raise "unimplemented yet"))))
