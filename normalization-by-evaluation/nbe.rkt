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

(define (do-application fun arg)
  (match fun
    [(CLOS env x b)
     (val (extend env x arg) b)]
    [neutral-fun
     (N-application fun arg)]))

(define (run-program env exprs)
  (match exprs
    [(list) (void)]
    [(list `(define ,x ,e) rest ...)
     (let ([v (val env e)])
       (run-program (extend env x v) rest))]
    [(list e rest ...)
     (displayln (norm env e))
     (run-program env rest)]))

(define (add-* x) (string->symbol (string-append (symbol->string x) "*")))
(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))

;;; Expressions equated by zero or more α and β steps are called αβ-equivalent.
; α-renaming and β-reduction
;;; grammar
; normal-form ::= <neutral>
;               | (λ (<id>) <normal-form>)
; neutral ::= <id>
;           | ( <neutral> <normal-form> )
;;; neutral variable
(struct N-var (name))
;;; neutral application
(struct N-application (rator rand))

(define (read-back used-names v)
  (match v
    [(CLOS env x body)
     (let* ([y (freshen used-names x)]
            [neutral-y (N-var y)])
       `(λ (,y)
          ,(read-back (cons y used-names)
                      (val (extend env x neutral-y) body))))]
    [(N-var x) x]
    [(N-application rator rand)
     `(,(read-back used-names rator)
       ,(read-back used-names rand))]))

(define (norm env e)
  (read-back '() (val env e)))
