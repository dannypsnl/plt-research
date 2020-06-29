#lang nanopass

(module+ test
  (require rackunit))

(define-language L0
  (terminals
   (variable (x))
   (primitive (pr))
   (datum (d))
   (constant (c)))
  (Expr (e body)
        x ;;; variable
        pr ;;; primitive functions
        c ;;; constant(literal)
        'd ;;; quoted is datum
        (begin e* ... e)
        (if e0 e1)
        (if e0 e1 e2)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...) body* ... body)
        (letrec ([x* e*] ...) body* ... body)
        (e0 e1 ...)))

(define variable? (lambda (x) (symbol? x)))
(define primitive?
  (lambda (x)
    (memq x '(+ - * / cons car cdr pair? vector make-vector vector-length vector-ref vector-set! vector? string make-string string-length string-ref string-set! string? void))))
(define datum? (lambda (x) #t))
(define constant?
  (lambda (x)
    (or (number? x)
        (char? x)
        (string? x))))

(define-language L1
  ;;; base on which language
  (extends L0)
  (terminals
   (- (constant (c))))
  (Expr (e body)
        (- c
           (if e0 e1)
           (lambda (x* ...) body* ... body)
           (let ([x* e*] ...) body* ... body)
           (letrec ([x* e*] ...) body* ... body))
        (+ (lambda (x* ...) body)
           (let ([x* e*] ...) body)
           (letrec ([x* e*] ...) body))))

(define-pass make-explicit : L0 (ir) -> L1 ()
  (definitions)
  (Expr : Expr (ir) -> Expr ()
        [,x x]
        [,pr pr]
        [,c `',c]
        [',d `',d]
        [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
        ;;; c: condition, t: then-expression, e: else-expression
        ;;; if c t e|if c t => if c t e
        [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))]
        [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
        [(lambda (,x* ...) ,[body*] ... ,[body])
         `(lambda (,x* ...) (begin ,body* ... ,body))]
        [(let ([,x* ,[e*]] ...) ,[body*] ... ,[body])
         `(let ([,x* ,e*] ...) (begin ,body* ... ,body))]
        [(letrec ([,x* ,[e*]] ...) ,[body*] ... ,[body])
         `(letrec ([,x* ,e*] ...) (begin ,body* ... ,body))]
        [(,[e0] ,[e1] ...) `(,e0 ,e1 ...)])
  (Expr ir))

(define-pass a-lambda : (L0 Expr) (e) -> * (string)
  (Expr : Expr (e) -> * (string)
        [(lambda (,x* ...) ,body* ...) "a lambda"]
        [else (format "want a lambda but got: ~a" e)])
  (Expr e))

(module+ test
  (define-parser parse-L0 L0)
  (parse-L0 '(let ([x 19]) (f x) (g x 4)))
  (unparse-L0 (parse-L0 '(let ([x 19]) (f x) (g x 4))))

  (a-lambda (parse-L0 `(lambda (x) x))))
