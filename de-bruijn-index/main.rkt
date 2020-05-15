#lang typed/racket

(struct term [])
(struct term:var term [(v : String)])
(struct term:lambda term [(v : String) (body : term)])
(struct term:application term [(t1 : term) (t2 : term)])

(module+ test
  (require rackunit))

(module+ test
 (check-equal? (+ 2 2) 4))
