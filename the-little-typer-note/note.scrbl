#lang scribble/manual

@(require (for-label (only-meta-in 0 pie))
          racket/sandbox scribble/example
          (for-syntax racket/base syntax/parse))

@(define ev
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'pie)))

@(define-syntax (ex stx)
   (syntax-parse stx
     [(_ e ...)
      (syntax/loc stx
        (examples #:eval ev e ...))]))

@(define-syntax (pie stx)
   (syntax-parse stx
     [(_ e ...)
      (syntax/loc stx
        (racket e ...))]))

@(define-syntax (pieblock stx)
   (syntax-parse stx
     [(_ e ...)
      (syntax/loc stx
        (racketblock e ...))]))

@(define-syntax (def-type-constructor stx)
   (syntax-parse stx
     [(_ con:id content ...)
      (syntax/loc stx
        (defform #:kind "type constructor" #:id con con content ...))]
     [(_ con-expr content ...)
      (syntax/loc stx
        (defform #:kind "type constructor" con-expr content ...))]))


@(define-syntax (def-constructor stx)
   (syntax-parse stx
     [(_ con:id content ...)
      (syntax/loc stx
        (defthing #:kind "constructor" con content ...))]
     [(_ con-expr content ...)
      (syntax/loc stx
        (defproc #:kind "constructor" con-expr content ...))]))


@(define-syntax (def-eliminator stx)
   (syntax-parse stx
     [(_ elim-expr content ...)
      (syntax/loc stx
        (defproc #:kind "eliminator" elim-expr content ...))]))


@title{NOTE The Little Typer}

@section{Judgement}

The four forms of Judgement.

1. <id:value> is a <id:type>. e.g. @ex[(the Atom 'vsei)]
2. <id:value> is the same <id:type> as <id:value>. e.g. @ex[(check-same Nat 1 1)]
3. <id:type> is a type. e.g. @ex[(the U Nat)]
4. <id:type> and <id:type> are the same type. e.g. @ex[(check-same U Nat Nat)]

@pie[U] is Universe, @pie[Nat] is natural

@section{eliminator}

@pie[which-Nat] is an eliminator of @pie[Nat], it takes three parameters: @pie[target], @pie[base] and @pie[step], here are examples:

@ex[
(which-Nat 0
  'u
  (lambda (_n) 's))
(which-Nat 10
  'u
  (lambda (_n) 's))
]

@pie[(add1 _n)] will be @pie[target]
