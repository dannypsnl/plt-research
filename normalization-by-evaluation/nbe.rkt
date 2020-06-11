#lang racket
;;; NOTE: totally from http://davidchristiansen.dk/tutorials/nbe/

;;; closure
(struct CLOS
  (env var body)
  #:transparent)

(define (extend env x v)
  (cons (cons x v) env))

(define (val env exp)
  (match exp
    [`(λ (,x) ,b)
     (CLOS env x b)]
    [x #:when (symbol? x)
       (let ([xv (assv x env)])
         (if xv
             (cdr xv)
             (error 'val "unknown variable ~a" x)))]
    [`(,rator ,rand)
     (do-application
      (val env rator)
      (val env rand))]))

(define (do-application clos arg)
  (match clos
    [(CLOS env x b)
     (val (extend env x arg) b)]))

(define (run-program env exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (val env e)])
       (run-program (extend env x v) rest))]
    [(cons e rest)
     (displayln (val env e))
     (run-program env e)]))

(define (add-* x) (string->symbol (string-append (symbol->string x) "*")))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))
