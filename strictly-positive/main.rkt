#lang racket

(module check racket
  (provide data)
  (require syntax/parse/define
           (for-syntax racket/match
                       racket/list))

  (begin-for-syntax
    ; strictly positive check
    ; @name: name of data type
    ; @c: type of constructor
    (define (check name c positive?)
      (define n (syntax->datum name))
      (define (check-left-right t1 t2)
        (cond
          ; endofunctors are positive
          [(equal? t1 t2) (void)]
          ; self at negative
          [(and (equal? (if (symbol? t1) t1 (first t1)) n)
                (not positive?))
           (raise-syntax-error 'negative "bad data type"
                               name)])
        (check name t1 (not positive?))
        (check name t2 positive?))
      (match c
        [`(Pi (,name : ,t1) ,t2)
         (check-left-right t1 t2)]
        [`(-> ,t1 ,t2)
         (check-left-right t1 t2)]
        [x (void)]))

    (define-syntax-class bind
      (pattern (name:id : ty)
               #:attr lam
               (λ (t) #`(Pi [name : ty] #,t))))
    (define-syntax-class constructor
      (pattern (name b*:bind ... : ty)
               #:attr desugar-type
               (syntax->datum
                (foldr (λ (n r)
                         (n r))
                       #'ty
                       (attribute b*.lam))))))

  (define-syntax-parser data
    [(_ name:id c*:constructor ...)
     (for ([c (attribute c*.desugar-type)])
       (check #'name c #t))
     #''ok]
    [(_ (name:id d*:bind ...) c*:constructor ...)
     (for ([c (attribute c*.desugar-type)])
       (check #'name c #t))
     #''ok]))

(require 'check)

(data Nat
      [z : Nat]
      [s [n : Nat] : Nat])
(data (Vec [A : Type] [len : Nat])
      [vecnil : (Vec A 0)]
      [vec:: [a : A] [v : (Vec A n)] : (Vec A (s n))])

; neg
#;(data Bad
        [bad [x : (-> Bad X)] : Bad])
