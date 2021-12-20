#lang racket

(define (show v cur-scope-level)
  (printf "~a at ~a~n" v cur-scope-level))

(define (print-var*-scope prog [cur-scope-level 0])
  (match prog
    [`(define ,v : ,ty ,exp)
     (show v cur-scope-level)
     (print-var*-scope exp (+ cur-scope-level 1))]
    ;;; exp
    [`(lambda ([,p* : ,ty*] ...) : ,ty ,e* ... ,e)
     (for ([p p*])
       (show p (- cur-scope-level 1)))
     (for ([e e*])
       (print-var*-scope e (+ cur-scope-level 1)))
     (print-var*-scope e (+ cur-scope-level 1))]
    [rest (void)]))

(print-var*-scope
 '(define add-const : (-> i32 i32 i32)
    (lambda ([x : i32]) : (-> i32 i32)
      (lambda ([y : i32]) : i32
        (+ x y)))))
