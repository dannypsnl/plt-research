#lang typed/racket

(require "term.rkt"
         "value.rkt"
         "quote.rkt")

(define-type Γ (Mutable-HashTable name value))

(: type-inferable (->* (Γ inferable-term) (Integer)
                       ; String stands for error in case
                       (U value String)))
(define (type-inferable ctx term [i 0])
  (let/cc throw : String
    (match term
      [(t:annotation e p)
       (let ([err (type-checkable ctx i p (v:*))])
         (when (string? err) (throw err)))
       (define type : value
         (eval-checkable p empty))
       (let ([err (type-checkable ctx i e type)])
         (when (string? err) (throw err)))
       type]
      [(t:*) (v:*)]
      [(t:free name) (hash-ref ctx name
                               (λ () "unknown identifier"))]
      [(t:Π p pp)
       (let ([err (type-checkable ctx i p (v:*))])
         (when (string? err) (throw err)))
       (define sr (eval-checkable p empty))
       (hash-set! ctx (name:local i) sr)
       (type-checkable ctx (+ 1 i)
                       (subst-checkable 0 (t:free (name:local i))
                                        pp)
                       (v:*))
       (v:*)]
      [(t:app e ep)
       (let ([ty+err (type-inferable ctx e i)])
         (when (string? ty+err) (throw ty+err))
         (match ty+err
           [(v:Π t tp)
            (let ([err (type-checkable ctx i ep t)])
              (when (string? err) (throw err)))
            (tp (eval-checkable ep empty))]
           [_ "illegal application"]))])))
(define (type-checkable [ctx : Γ]
                        [i : Integer]
                        [term : checkable-term]
                        [type : value])
  ; String stands for error in case
  : (U Void String)
  (match* (term type)
    [((t:λ checkable)
      (v:Π s sp))
     (hash-set! ctx (name:local i)
                s)
     (type-checkable ctx
                     (+ 1 i)
                     (subst-checkable 0
                                      (t:free (name:local i))
                                      checkable)
                     (sp (vfree (name:local i))))]
    [(inferable v)
     (define vp (type-inferable ctx (cast inferable inferable-term) i))
     (cond
       [(string? vp) vp]
       [vp (if (not (eqv? (quote vp) (quote v)))
               (format "type mismatch, want: %s but got: %s" vp v)
               (void))])]
    [(_ _) "type mismatch"]))

(define (subst-inferable [i : Integer]
                         [r : inferable-term]
                         [t : inferable-term])
  : inferable-term
  (match t
    [(t:annotation c ty)
     (t:annotation (subst-checkable i r c)
                   (subst-checkable i r ty))]
    [(t:Π t1 t2)
     (t:Π (subst-checkable i r t1)
          (subst-checkable (+ 1 i) r t2))]
    [(t:bound j)
     (if (= i j)
         r
         (t:bound j))]
    [(t:*) (t:*)]
    [(t:free name) (t:free name)]
    [(t:app inferable c)
     (subst-inferable i r (t:app inferable
                                 (subst-checkable i r c)))]))
(define (subst-checkable [i : Integer]
                         [r : inferable-term]
                         [c : checkable-term])
  : checkable-term
  (match c
    [(t:λ c)
     (t:λ (subst-checkable (+ 1 i) r c))]
    [inferable
     (subst-inferable i r (cast inferable inferable-term))]))

(define (eval-inferable [t : inferable-term]
                        [v* : (Listof value)])
  : value
  (define (vapp [t : value] [v : value]) : value
    (match t
      [(v:λ f) (f v)]
      [(v:neutral neu)
       (v:neutral (neu:app neu v))]))
  (match t
    [(t:annotation c _)
     (eval-checkable c v*)]
    [(t:app f arg)
     (vapp (eval-inferable f v*)
           (eval-checkable arg v*))]
    [(t:Π t1 t2)
     (v:Π (eval-checkable t1 v*)
          (λ ([x : value])
            (eval-checkable t2 (list* x v*))))]
    [(t:bound i) (list-ref v* i)]
    [(t:free name) (vfree name)]
    [(t:*) (v:*)]))
(define (eval-checkable [t : checkable-term]
                        [v* : (Listof value)])
  : value
  (match t
    [(t:λ checkable)
     (v:λ (λ ([x : value])
            (eval-checkable checkable (#{cons @ value} x v*))))]
    [inferable
     (eval-inferable (cast inferable inferable-term)
                     v*)]))
