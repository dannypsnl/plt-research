#lang semilit racket/base

From https://dvanhorn.github.io/redex-aam-tutorial/

> (require redex)

> (term 2)
2
> (term fred)
'fred
> (term ())
'()

> (define-language L
>   (M ::= N F (M ...))
>   (F ::= fred wilma)
>   (N ::= 2 7))

> (redex-match? L N (term 2))
#t
> (redex-match? L N (term 9))
#f
> (redex-match? L M (term (((((((fred)))))))))
#t

Arbitrary pattern

> (redex-match? L (N ...) (term (2 7 7 2 7 2)))
#t
> (redex-match? L (M_1 M_2) (term (7 (2 fred))))
#t

subscript of pattern is required if we want to distinguish them

> (redex-match? L (M M) (term (7 (2 fred))))
#f
> (redex-match? L (M M) (term ((2 fred) (2 fred))))
#t

> (define-metafunction L
>   swap : M -> M
>   [(swap fred) wilma]
>   [(swap wilma) fred]
>   [(swap (M ...)) ((swap M) ...)]
>   [(swap M) M])

> (term (swap (wilma fred)))
'(fred wilma)
> (quote (swap (wilma fred)))
'(swap (wilma fred))
> (term (7 (swap (wilma 2 (fred)))))
'(7 (fred 2 (wilma)))

> (define-language PCF
>   (M ::=
>      N O X L
>      (μ (X : T) L)
>      (M M ...)
>      (if0 M M M))
>   (X ::= variable-not-otherwise-mentioned)
>   (L ::= (λ ([X : T] ...) M))
>   (V ::= N O L)
>   (N ::= number)
>   (O ::= O1 O2)
>   (O1 ::= add1 sub1)
>   (O2 ::= * +)
>   (T ::= num (T ... -> T)))
> (define-term fact-5
>    ((μ (fact : (num -> num))
>        (λ ([n : num])
>          (if0 n
>               1
>               (* n (fact (sub1 n))))))
>     5))
> (test-equal (redex-match? PCF M (term fact-5)) #t)

Type

> (define-language REDEX)
> (define-judgment-form REDEX
>    #:mode (lookup I I O)
>    #:contract (lookup ((any any) ...) any any)
>    [(lookup (_ ... (any any_0) _ ...) any any_0)])
> (define-relation REDEX
>    unique ⊆ (any ...)
>    [(unique (any_!_1 ...))])
> (define-metafunction REDEX
>    ext1 : ((any any) ...) (any any) -> ((any any) ...)
>    [(ext1 (any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
>     (any_0 ... (any_k any_v1) any_1 ...)]
>    [(ext1 (any_0 ...) (any_k any_v1))
>     ((any_k any_v1) any_0 ...)])
> (define-metafunction REDEX
>    ext : ((any any) ...) (any any) ... -> ((any any) ...)
>    [(ext any) any]
>    [(ext any any_0 any_1 ...)
>     (ext1 (ext any any_1 ...) any_0)])

> (define-extended-language PCFT PCF
>   (Γ ::= ((X T) ...)))
> (define-judgment-form PCFT
>    #:mode (⊢ I I I O)
>    #:contract (⊢ Γ M : T)
>    [(lookup Γ X T)
>     -------------- var
>     (⊢ Γ X : T)]
>    [------------- num
>     (⊢ Γ N : num)]
>    [----------------------- op1
>     (⊢ Γ O1 : (num -> num))]
>    [--------------------------- op2
>     (⊢ Γ O2 : (num num -> num))]
>    [(⊢ Γ M_1 : num)
>     (⊢ Γ M_2 : T)
>     (⊢ Γ M_3 : T)
>     --------------------------- if0
>     (⊢ Γ (if0 M_1 M_2 M_3) : T)]
>    [(⊢ (ext Γ (X T)) L : T)
>     ----------------------- μ
>     (⊢ Γ (μ (X : T) L) : T)]
>    [(⊢ Γ M_0 : (T_1 ..._1 -> T))
>     (⊢ Γ M_1 : T_1) ...
>     ----------------------- app
>     (⊢ Γ (M_0 M_1 ..._1) : T)]
>    [(unique (X ...))
>     (⊢ (ext Γ (X T) ...) M : T_n)
>     ------------------------------------------ λ
>     (⊢ Γ (λ ([X : T] ...) M) : (T ... -> T_n))])

> (judgment-holds (⊢ () (λ ([x : num]) x) : (num -> num)))
#t
> (judgment-holds (⊢ () (λ ([x : num]) x) : T) T)
'((num -> num))
> (judgment-holds (⊢ () fact-5 : T) T)
'(num)

We even can show computation process

> (show-derivations
>   (build-derivations
>    (⊢ () (λ ([x : num]) (add1 x)) : T)))
