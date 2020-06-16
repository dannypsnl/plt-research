#lang typed/racket

(require "term.rkt"
         "value.rkt")

(define (transform-k [t : term]
                     ; k stands for higher continuation(via host language)
                     [k : (atom-value → complex-value)])
  : complex-value
  (if (atom-term? t)
      (k (m t))
      (let* ([rv (symbol->string (gensym "rv"))]
             [new-k (atom:λ
                     (list rv)
                     (k (atom:var rv)))])
        (match t
          [(term:if condition then else)
           (transform-k
            condition
            (λ (a-cond)
              (complex:if a-cond
                          (transform-c then new-k)
                          (transform-c else new-k))))]
          [(term:ap _ _)
           (transform-c t new-k)]))))
(define (transform-c [t : term]
                     [k : atom-value])
  : complex-value
  (if (atom-term? t)
      (complex:ap k (list (m t)))
      (let ([new-k (symbol->string (gensym "k"))])
        (match t
          [(term:if condition then else)
           (complex:ap
            (atom:λ
             (list new-k)
             (transform-k
              condition
              (λ (a-cond)
                (complex:if
                 a-cond
                 (transform-c then (atom:var new-k))
                 (transform-c else (atom:var new-k))))))
             (list k))]
          [(term:ap f args)
           (transform-k
            f
            (λ (fs)
              (transform-k*
               args
               (λ (es)
                 (complex:ap fs (append es (list k)))))))]))))
(define (transform-k* [term* : (Listof term)]
                      [k : ((Listof atom-value) → complex-value)])
  : complex-value
  (if (empty? term*)
      (k '())
      (transform-k
       (car term*)
       (λ (hd)
         (transform-k*
          (cdr term*)
          (λ (t1)
            (k (cons hd t1))))))))
(define (m [t : term])
  : atom-value
  (match t
    [(term:λ params body)
     (define k (symbol->string (gensym "k")))
     (atom:λ (append params (list k))
             (transform-c body (atom:var k)))]
    [(term:var x) (atom:var x)]
    [(term:int-literal i) (atom:int-literal i)]
    [(or (term:call/cc)
         (term:call/ec))
     (atom:λ
      (list "f" "cc")
      (complex:ap
       (atom:var "f")
       (list (atom:λ
              (list "x" "_")
              (complex:ap (atom:var "cc")
                          (list (atom:var "x"))))
             (atom:var "cc"))))]
    [_ (error "not atomic term")]))

(define (atom-term? [t : term])
  : Boolean
  (match t
    [(or (term:var _)
         (term:λ _ _)
         (term:int-literal _)
         (term:call/cc)
         (term:call/ec)) #t]
    [_ #f]))

(module+ test
  (require racket/pretty)
  (pretty-print
   (transform-c
    (term:ap (term:var "g") (list (term:var "a")))
    (atom:var "halt")))
  (pretty-print
   (transform-c
    (term:ap
     (term:call/cc)
     (list (term:λ
            (list "halt")
            (term:ap (term:var "halt")
                     (list (term:int-literal 5))))))
    (atom:var "halt"))))
