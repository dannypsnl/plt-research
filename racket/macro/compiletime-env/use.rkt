;; Dependency:
;;   add depends on         {env}
;;   mul depends on     {add,env}
;;   use depends on {mul,add,env}
#lang racket/base

(require (for-syntax racket/base
                     "env.rkt")
         "mul.rkt")

(begin-for-syntax
  (printf "use.rkt: my-fun = ~s\n" (env-ref #'my-fun))
  (printf "use.rkt: has-key?(your-fun) = ~s\n" (env-has-id? #'your-fun))
  )
(my-fun 8 3)

#;
(begin
  (require (only-in "add.rkt" [my-fun add:my-fun]))
  (begin-for-syntax
    (printf "use.rkt: add:my-fun = ~s\n" (env-ref #'add:my-fun))
    )
  (add:my-fun 7 (my-fun 8 3))
  )
