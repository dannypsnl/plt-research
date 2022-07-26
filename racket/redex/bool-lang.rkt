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

(define-language bool-standard-lang
  (B true
     false
     (or B B))
  (E (or E B)
     hole))

(define bool-standard-red
  (reduction-relation
   bool-standard-lang
   (--> (in-hole E (or true B))
        (in-hole E true)
        "or-true")
   (--> (in-hole E (or false B))
        (in-hole E B)
        "or-false")))

(traces bool-standard-red
        (term (or (or true false) (or true true))))
