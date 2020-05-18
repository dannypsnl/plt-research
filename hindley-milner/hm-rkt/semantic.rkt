#lang typed/racket

(require "lang.rkt"
         "typ.rkt")

(struct Context
  [(freevar-counter : Integer)]
  #:transparent
  #:mutable)
(define (Context/new)
  (Context 0))
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
    (_ (raise "unimplemented yet"))))
