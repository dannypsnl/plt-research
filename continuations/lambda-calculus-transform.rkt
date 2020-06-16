#lang racket

; T stands for transform
(define (T exp k)
  (match exp
    ; take a continuation as a new parameter for existed lambda
    ; and transform body expression with such continuation
    [`(λ (,var) ,expr)
     (define $k (gensym '$k))
     `(,k (λ (,var ,$k) ,(T expr $k)))]
    ; variable is variable
    [(? symbol?) `(,k ,exp)]
    ; application seems extremely complex at first
    ; but it was quite straightforward thinking
    ; 1. f and e both are original terms, which means need to convert them with T, else they cannot handle continuation argument
    ; then how to construct continutaion for f
    ; 2. the answer is a lambda takes result of f, with transformed e as body
    ; for e(body)?
    ; 3. still takes result of e, but with application about **T result of f**, **T result of e**, and continuation
    [`(,f ,e)
     (define $f (gensym '$f))
     (define $e (gensym '$e))
     (T f `(λ (,$f)
             ,(T e `(λ (,$e)
                      (,$f ,$e ,k)))))]))
; result should be something like: (λ (a $k32194) ($k32194 a))
(printf "v1: ~a~n" (T '(λ (a) a) 'halt))
; but this can be complex, be like: ((λ ($f39527) ((λ ($e39528) ($f39527 $e39528 halt)) a)) g)
(printf "v1: ~a~n" (T '(g a) 'halt))
; because current T, simply assuming func and exp are complex(a.k.a λ) terms which not, produce noisy result.

; improvement:
;   give T/v2 a high-order continuation, which means a λ on host language.
;   this would work, because if the expression to be transformed is atomic, no need to bind it with a fresh variable.
(define (T/v2 exp k)
  (match exp
    ; variable
    [(? symbol?) (k `,exp)]
    ; λ
    [`(λ (,var) ,expr)
     (define $k (gensym '$k))
     (k `(λ (,var ,$k) ,(T/v2 expr (λ (rv) `(,$k ,rv)))))]
    [`(,f ,e)
     (define $rv (gensym '$rv))
     (define cont `(λ (,$rv) ,(k $rv)))
     (T/v2 f (λ ($f)
               (T/v2 e (λ ($e)
                         `(,$f ,$e ,cont)))))]))
; now, result is: (g a (λ ($rv90115) (halt $rv90115)))
(printf "v2: ~a~n" (T/v2 '(g a) (λ (a) `(halt ,a))))

; but, still a little bit noisy. That's right! we always want to make thing easier!
(define (M exp)
  (match exp
    ; take one more parameter as continuation is required for every λ
    [`(λ (,var) ,expr)
     (define $k (gensym '$k))
     `(λ (,var ,$k) ,(T-c expr $k))]))
(define (T-k exp k)
  (match exp
    ; var is var
    [(? symbol?) (k `,exp)]
    [`(λ . ,_) (k (M exp))]
    [`(,f ,e)
     (define $rv (gensym '$rv))
     (define cont `(λ (,$rv) ,(k $rv)))
     (T-k f (λ ($f)
              (T-k e (λ ($e)
                       `(,$f ,$e ,cont)))))]))
(define (T-c exp c)
  (match exp
    ; var is var
    [ (? symbol?) `(,c ,exp)]
    [`(λ . ,_) `(,c ,(M exp))]
    [`(,f ,e)
     (T-k f (λ ($f)
              (T-k e (λ ($e)
                       `(,$f ,$e ,c)))))]))
; now we combine all good part from v1 and v2
(printf "v3: ~a~n" (T-c '(g a) 'halt))
