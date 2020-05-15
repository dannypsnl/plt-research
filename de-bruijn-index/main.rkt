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
(: convert (->* [term] [(Immutable-HashTable String Integer)] bterm))
(define (convert t [rename-to (make-immutable-hash '())])
  (match t
    ;; get index from environment
    ([term:var name] (bterm:var (hash-ref rename-to name)))
    ([term:lambda p b]
     (bterm:lambda
      (convert b
               ;; bind parameter name to an index
               (hash-set rename-to p (hash-count rename-to)))))
    ([term:application t1 t2]
     (bterm:application
      (convert t1 rename-to)
      (convert t2 rename-to)))))

(module+ test
  (require typed/rackunit))

(module+ test
  (check-equal? (convert (term:lambda "x" (term:var "x")))
                (bterm:lambda (bterm:var 0)))
  (check-equal? (convert (term:lambda "f"
                                      (term:application
                                       (term:lambda "x"
                                                    (term:application (term:application (term:var "f")  (term:var "x")) (term:var "x")))
                                       (term:lambda "x"
                                                    (term:application (term:application (term:var "f")  (term:var "x")) (term:var "x"))))))
                (bterm:lambda
                 (bterm:application
                  (bterm:lambda
                   (bterm:application
                    (bterm:application
                     (bterm:var 0) (bterm:var 1)) (bterm:var 1)))
                  (bterm:lambda
                   (bterm:application
                    (bterm:application
                     (bterm:var 0) (bterm:var 1)) (bterm:var 1))))))
  )
