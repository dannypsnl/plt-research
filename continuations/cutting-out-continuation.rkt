#lang nanopass

(define-language L
  (terminals
   (integer (int)))
  (Expr (expr)
        int
        [add expr expr1]))

(define-pass eval* : (L Expr) (e c) -> (L Expr) ()
  (f : Expr (e) -> Expr ()
     [,int (c int)]
     [(add ,expr ,expr1)
      (eval* expr (next expr1 c))])
  (f e))

(define (halt x) x)
(define (next y c)
  (λ (n)
    (eval* y (add n c))))
(define (add n c)
  (λ (m)
    (c (+ n m))))

(define/match (exec cont n)
  [('HALT n) n]
  [(`(NEXT ,y ,c) n) (eval** y `(ADD ,n ,c))]
  [(`(ADD ,n ,c) m) (exec c (+ n m))])

(define-pass eval** : (L Expr) (e c) -> * ()
  (f : Expr (e) -> * ()
     [,int (exec c int)]
     [(add ,expr ,expr1)
      (eval** expr `(NEXT ,expr1 ,c))])
  (f e))

(define (eval x) (eval** x 'HALT))

(module+ test
  (require rackunit)

  (define-parser parse L)

  (check-equal? (eval* (parse '(add 1 2)) halt)
                (eval (parse '(add 1 2)))))
