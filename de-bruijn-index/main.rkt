#lang typed/racket

(struct term [] #:transparent)
(struct term:var term [(v : String)] #:transparent)
(struct term:lambda term [(v : String) (body : term)] #:transparent)
(struct term:application term [(t1 : term) (t2 : term)] #:transparent)

(struct bterm [] #:transparent)
(struct bterm:var bterm [(v : Integer)] #:transparent)
(struct bterm:lambda bterm [(body : bterm)] #:transparent)
(struct bterm:application bterm [(t1 : bterm) (t2 : bterm)] #:transparent)

;;; convert lambda calculus to de-bruijn index form
(: convert (-> term bterm))
(define (convert t)
  (match t
    ([term:var v] (bterm:var 0))
    ([term:lambda p b]
      (bterm:lambda
        (convert b)))
    ([term:application t1 t2]
      (bterm:application
        (convert t1)
        (convert t2)))))

(module+ test
  (require typed/rackunit))

(module+ test
 (check-equal? (convert (term:lambda "x" (term:var "x")))
               (bterm:lambda (bterm:var 0)))
 )
