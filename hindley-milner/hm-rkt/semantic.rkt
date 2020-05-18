#lang typed/racket

(require "lang.rkt"
         "typ.rkt")

(: type/infer (-> expr typ))
(define (type/infer exp)
  (match exp
    ;;; some expressiones are easy to guess what it is
    ; in our **langauge** is `int`, `bool` and `string`
    ([expr:int _] (typ:builtin "int"))
    ([expr:bool _] (typ:builtin "bool"))
    ([expr:string _] (typ:builtin "string"))
    (_ (raise "unimplemented yet"))))
