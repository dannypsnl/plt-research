#lang typed/racket

(require "lang.rkt"
         "typ.rkt")

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
    (typ:freevar cur-count)))

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
                         (map (λ (_) (Context/new-freevar! ctx)) params))]
           [body-typ (type/infer body ctx)])
       (typ:arrow param-types body-typ)))
    (_ (raise "unimplemented yet"))))
