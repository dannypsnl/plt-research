#lang racket

(provide LC r-app)

(require redex)

(define-language LC
  (E X
     (λ (X ...) E)
     (E E ...))
  (X variable-not-otherwise-mentioned))

(define r-app
  (reduction-relation
   LC #:domain E
   (--> ((λ (X ...) E_body) E_args ...)
        (subst ,(map list (term (E_args ...)) (term (X ...)))
               E_body)
        "app")))

(define-metafunction LC
  subst : ((any X) ...) any -> any
  [(subst [(any_1 X_1) ... (any_X X) (any_2 X_2) ...] X) any_X]
  [(subst [(any_1 X_1) ...]  x) x]
  [(subst [(any_1 X_1) ...]  (lambda (X ...) any_body))
   (lambda (X_new ...)
     (subst ((any_1 X_1) ...)
            (subst-raw ((X_new x) ...) any_body)))
   (where  (X_new ...)  ,(variables-not-in (term any_body) (term (X ...))))]
  [(subst [(any_1 X_1) ...]  (any ...)) ((subst [(any_1 X_1) ...]  any) ...)]
  [(subst [(any_1 X_1) ...]  any_*) any_*])
(define-metafunction LC
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((X_n1 X_o1) ... (X_new x) (X_n2 X_o2) ...) x) X_new]
  [(subst-raw ((X_n1 X_o1) ...)  x) x]
  [(subst-raw ((X_n1 X_o1) ...)  (lambda (X ...) any))
   (lambda (X ...) (subst-raw ((X_n1 X_o1) ...)  any))]
  [(subst-raw [(any_1 X_1) ...]  (any ...))
   ((subst-raw [(any_1 X_1) ...]  any) ...)]
  [(subst-raw [(any_1 X_1) ...]  any_*) any_*])
