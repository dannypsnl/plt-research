#lang racket
(require (for-syntax syntax/parse))

(define (reify-name n)
  (string->symbol
   (string-append "_" (number->string n))))
(struct fresh (value count)
  #:methods gen:custom-write
  [(define (write-proc self out mode)
     (if (fresh-value self)
         (display (fresh-value self) out)
         (display (reify-name (fresh-count self)) out)))]
  #:mutable)

(define (force-fresh f)
  (cond
    [(and (fresh? f) (fresh-value f))
      (force-fresh (fresh-value f))]
    [(fresh? f) (reify-name (fresh-count f))]
    [(list? f) (map force-fresh f)]
    [else f]))
(define (occurs? fresh in-b)
  (cond
    [(list? in-b) (ormap (lambda (v) (occurs? fresh v)) in-b)]
    [else
      ; compare the reference to check if occurs
      (eq? fresh in-b)]))
(define (unify? a b)
  (cond
    [(empty? a) (empty? b)]
    [(and (list? a) (list? b))
     (and (unify? (car a) (car b))
          (unify? (cdr a) (cdr b)))]
    [(fresh? a)
     (if (fresh-value a)
         (unify? (fresh-value a) b)
         ; check they are not refer to the same object
         (if (eq? a b) ; for example, `unify? a a`
             a
             (if (occurs? a b)
                #f
                (begin
                  (set-fresh-value! a b)
                  #t))))]
    [(fresh? b)
     (unify? b a)]
    [(and (symbol? a) (symbol? b))
     (eq? a b)]
    [else #f]))

(define-syntax (run* stx)
  (define (parse-goal query stx)
    (syntax-parse stx
      #:datum-literals
      (succeed fail == fresh)
      [fail #'(list)]
      [succeed #`(list #,query)]
      [(== a b)
       #`(if (unify? a b)
             (list #,query)
             (list))]
      [(fresh (x:id) goal)
       #`(let ()
           (define x (make-fresh))
           #,(parse-goal query #'goal))
       ]
      ))
  
  (syntax-parse stx
    #:datum-literals
    (succeed fail)
    [(_ query:id goal)
     #`(let ()
         (define count 0)
         (define (make-fresh)
           (define v (fresh #f count))
           (set! count (add1 count))
           v)
         (define query (make-fresh))
         (map force-fresh #,(parse-goal #'query #'goal)))]
    ))

(run* q
  (== `(,q) q))

(module+ test
  (require rackunit)

  (check-equal?
    (run* q
      fail)
    '())
  
  (check-equal?
    (run* q
      succeed)
    '(_0))

  (check-equal?
    (run* q
      (== 'pea 'pod))
    '())
  
  (check-equal?
    (run* q (== q 'pea))
    '(pea))

  (check-equal?
    (run* q (== 'pea q))
    '(pea))

  (check-equal?
    (run* q (== q q))
    '(_0))

  (check-equal?
    (run* q (fresh (x) (== 'pea q)))
    '(pea))
  
  (check-equal?
    (run* q (fresh (x) (== 'pea x)))
    '(_0))
  
  (check-equal?
    (run* q (fresh (x) (== (cons x '()) q)))
    '((_1)))
  
  (check-equal?
    (run* q
      (== '(((pea)) pod)
          '(((pea)) pod)))
    '(_0))
  
  (check-equal?
    (run* q
      (fresh (x)
        (== `(,x ,x) q)))
    '((_1 _1)))

  (check-equal?
    (run* q
      (fresh (x)
             (== `(,x)
                 q)))
    '((_1)))
  
  (check-equal?
    (run* q
      (fresh (x)
        (== `(,q ,x) `(,x pod))))
    '(pod))
  )
