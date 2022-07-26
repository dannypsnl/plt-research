#lang racket
(require redex)
(module+ test
  (require rackunit))

(define-language bool-any-lang
  (B true
     false
     (or B B))
  (C (or B C)
     (or C B)
     hole))

(module+ test
  (check-not-false (redex-match bool-any-lang
                                B
                                (term (or false true))))
  (check-not-false (redex-match bool-any-lang
                                (in-hole C (or true B))
                                (term (or true (or true false))))))

(define bool-any-red
  (reduction-relation
   bool-any-lang
   (--> (in-hole C (or true B))
        (in-hole C true)
        "or-true")
   (--> (in-hole C (or false B))
        (in-hole C B)
        "or-false")))

(traces bool-any-red
        (term (or (or true false) (or true true))))
