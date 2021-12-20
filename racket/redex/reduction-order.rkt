#lang racket
(require redex)

(define-language LC
  (E X
     (λ (X ...) E)
     (E E ...))
  (X variable-not-otherwise-mentioned))
(define-extended-language LC+number
  LC
  (E ::= ....
     number
     O)
  (N number)
  (O O1 O-multi)
  (O1 add1 sub1)
  (O-multi + *))

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

(define r
  (reduction-relation
   LC+number #:domain E
   (--> ((λ (X ...) E_body) E_args ...)
        (subst ,(map list (term (E_args ...)) (term (X ...)))
               E_body)
        "app")
   (--> (+ N ...) ,(apply * (term (N ...)))
        "+")
   (--> (* N ...) ,(apply * (term (N ...)))
        "*")
   (--> (add1 N) ,(add1 (term N))
        "add1")
   (--> (sub1 N) ,(sub1 (term N))
        "sub1")))

(apply-reduction-relation r (term ((λ (e) e) 1)))

(define -->r (compatible-closure r LC+number E))

(traces -->r (term ((λ (x) (x x)) (λ (x) (x x)))))
(traces -->r (term ((λ (x) 3) ((λ (x) (x x)) (λ (x) (x x))))))
