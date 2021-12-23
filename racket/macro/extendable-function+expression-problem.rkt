#lang racket
(require syntax/parse/define)

(struct expr () #:transparent)
(struct e:int expr (int) #:transparent)
(struct e:add expr (e1 e2) #:transparent)
(struct e:sub expr (e1 e2) #:transparent)

(define m (make-hash))
(define-syntax-parser define/extendable
  [(_ name:id
      match-clause ...)
   (with-syntax ([n (car (generate-temporaries (list #'name)))])
     #'(begin
         (define (n e)
           (match e
             match-clause ...
             [else e]))
         (hash-set! m 'name (list n))
         (define (name e)
           ((apply compose (hash-ref m 'name)) e))))])
(define-syntax-parser define/extend
  [(_ name:id
      match-clause ...)
   (with-syntax ([n (car (generate-temporaries (list #'name)))])
     #'(begin
         (define (n e)
           (match e
             match-clause ...
             [else e]))
         (hash-set! m 'name (cons n (hash-ref m 'name)))))])

(define/extendable eval
  [(e:int i) i]
  [(e:add e1 e2) (+ (eval e1) (eval e2))])
(define/extend eval
  [(e:sub e1 e2) (- (eval e1) (eval e2))])

(define/extendable e->string
  [(e:int i) (number->string i)]
  [(e:add e1 e2) (format "(+ ~a ~a)" (e->string e1) (e->string e2))])
(define/extend e->string
  [(e:sub e1 e2) (format "(- ~a ~a)" (e->string e1) (e->string e2))])

(eval (e:add (e:int 1) (e:sub (e:int 3) (e:int 1))))
(e->string (e:add (e:int 1) (e:sub (e:int 3) (e:int 1))))
