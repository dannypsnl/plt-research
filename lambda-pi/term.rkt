#lang typed/racket

(provide (all-defined-out))

(define-type checkable-term
  (U
   inferable-term
   t:λ))
(define-type inferable-term
  (U
   t:annotation
   t:*
   t:Π
   t:bound
   t:free
   t:app))
(struct t:λ
  ([checkable : checkable-term])
  #:transparent)
(struct t:annotation
  ([c1 : checkable-term]
   [c2 : checkable-term]) #:transparent)
(struct t:* () #:transparent)
(struct t:Π
  ([t1 : checkable-term]
   [t2 : checkable-term]) #:transparent)
(struct t:bound ([i : Integer]) #:transparent)
(struct t:free ([name : name]) #:transparent)
(struct t:app
  ([func : inferable-term]
   [arg : checkable-term]) #:transparent)

(define-type name
  (U name:global name:local name:quote))
(struct name:global ([name : String]) #:transparent)
(struct name:local ([i : Integer]) #:transparent)
(struct name:quote ([i : Integer]) #:transparent)
(define (vfree [n : name]) : value
  (v:neutral (neu:free n)))


(define-type neutral (U neu:free neu:app))
(struct neu:free ([name : name]) #:transparent)
(struct neu:app
  ([func : neutral]
   [arg : value]) #:transparent)

(define-type value
  (U v:λ v:* v:Π v:neutral))
(struct v:λ ([func : (value → value)]) #:transparent)
(struct v:* () #:transparent)
(struct v:Π
  ([v : value]
   [func : (value → value)]) #:transparent)
(struct v:neutral ([neu : neutral]) #:transparent)
