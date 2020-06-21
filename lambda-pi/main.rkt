#lang typed/racket

(require "context.rkt"
         "term.rkt"
         "value.rkt"
         "quote.rkt")

(module+ test
  (require typed/rackunit))

(module+ test
  (define id (t:λ (t:bound 0)))
  (define const (t:λ (t:λ (t:bound 1))))
  (define term1 (t:app (t:annotation id (t:Π (t:free (name:global "a")) (t:free (name:global "a"))))
                       (t:free (name:global "y"))))

  (define ctx : Γ
    (make-hash (list (cons (name:global "y") (v:neutral (neu:free (name:global "a"))))
                     (cons (name:global "a") (v:*)))))

  (test-case
   "eval inferable"
   (check-equal? (quote (eval-inferable term1 empty))
                 (t:free (name:global "y"))))

  (test-case
   "type inferable"
   (check-equal? (type-inferable ctx term1)
                 (v:neutral (neu:free (name:global "a"))))))