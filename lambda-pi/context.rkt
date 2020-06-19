#lang typed/racket

(require "term.rkt"
         "value.rkt"
         "quote.rkt")

(define-type Γ (Mutable-HashTable name value))

(define (subst-inferable [i : Integer]
                         [r : inferable-term]
                         [t : inferable-term])
  : inferable-term
  (match t
    [(t:annotation c ty)
     (t:annotation (subst-checkable i r c)
                   (subst-checkable i r ty))]
    [(t:Π t1 t2)
     (t:Π (subst-checkable i r t1)
          (subst-checkable (+ 1 i) r t2))]
    [(t:bound j)
     (if (= i j)
         r
         (t:bound j))]
    [(t:*) (t:*)]
    [(t:free name) (t:free name)]
    [(t:app inferable c)
     (subst-inferable i r (t:app inferable
                                 (subst-checkable i r c)))]))
(define (subst-checkable [i : Integer]
                         [r : inferable-term]
                         [c : checkable-term])
  : checkable-term
  (match c
    [(t:λ c)
     (t:λ (subst-checkable (+ 1 i) r c))]
    [inferable
     (subst-inferable i r (cast inferable inferable-term))]))
