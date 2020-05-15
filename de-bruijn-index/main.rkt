#lang typed/racket

(module+ test
  (require typed/rackunit))

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
                     (bterm:var 0) (bterm:var 1)) (bterm:var 1)))))))

;;; beta-reduction
; if t1 is a λx.M then replace all x occurs in M with t2
; since bterm using de-bruijn index, lifting after beta-reduction is required
; to know which level we are, must count λ
(: beta-reduction (->* [bterm] [Integer (Immutable-HashTable Integer bterm)] bterm))
(define (beta-reduction t [de-bruijn-level 0] [subst : (Immutable-HashTable Integer bterm) #hash()])
  (match t
    ([bterm:var i]
     (hash-ref subst i (λ () t)))
    ([bterm:lambda body]
     (bterm:lambda (beta-reduction body (+ 1 de-bruijn-level) subst)))
    ([bterm:application t1 t2]
     (match t1
       ([bterm:lambda body]
        (let ([reduced-term (beta-reduction body (+ 1 de-bruijn-level)
                                            (hash-set subst de-bruijn-level t2))])
          ;;; dbi lifting by replace reduced-term (+ 1 dbi) with (var dbi)
          (beta-reduction reduced-term de-bruijn-level
                          (hash-set subst (+ 1 de-bruijn-level) (bterm:var de-bruijn-level)))))
       (_ (raise "cannot do application on non-lambda term"))))))

(module+ test
  ;;; λx.x
  ; => λ0
  ; beta-> λ0
  (check-equal? (beta-reduction (convert (term:lambda "x" (term:var "x"))))
                (bterm:lambda (bterm:var 0)))
  ;;; λx.(λy.λz.z x)
  ; => λ(λλ2 0)
  ; beta-> λλ2
  ; beta-> λλ1 (de-bruijn index lifting)
  (check-equal? (beta-reduction (convert (term:lambda "x"
                                                      (term:application
                                                       (term:lambda "y"
                                                                    (term:lambda "z" (term:var "z")))
                                                       (term:var "x")))))
                (bterm:lambda (bterm:lambda (bterm:var 1)))))