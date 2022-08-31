#lang typed/racket
(provide pretty-print-typ)
(require "typ.rkt")

(: pretty-print-typ (-> typ String))
(define (pretty-print-typ t)
  (match t
    ([typ:freevar idx subst]
     (if subst
         (pretty-print-typ subst)
         (format "?~a" idx)))
    ([typ:constructor name typ-args]
     (if (empty? typ-args)
         (format "~a" name)
         (let ([j (string-join (map
                                (λ ([typ-arg : typ])
                                  (pretty-print-typ typ-arg))
                                typ-args) " ")])
           (if (string=? name "pair")
               (format "(~a)" j)
               (format "(~a ~a)" name j)))))
    ([typ:arrow from to]
     (format "~a -> ~a" (pretty-print-typ from)
             (pretty-print-typ to)))))

(module+ test
  (require typed/rackunit))
(module+ test
  (check-equal? (pretty-print-typ (typ:builtin "int"))
                "int")
  (check-equal? (pretty-print-typ (typ:arrow (typ:constructor "pair" (list (typ:builtin "int"))) (typ:builtin "int")))
                "(int) -> int")
  (check-equal? (pretty-print-typ
                 (typ:arrow
                  (typ:constructor
                   "pair"
                   (list
                    (typ:constructor "list" (list (typ:freevar 0 #f)))
                    (typ:constructor "list" (list (typ:freevar 1 #f)))))
                  (typ:constructor "list" (list (typ:freevar 1 #f)))))
                "((list ?0) (list ?1)) -> (list ?1)"))
