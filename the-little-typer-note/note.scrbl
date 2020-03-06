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

@section{recursive is not an option}

To define a @pie[+] for @pie[Nat], we might first write down the following definition, unfortunately, we would get an error: @bold{Unknown variable +}

@pieblock[
(claim + (-> Nat Nat Nat))
(define +
  (lambda (n j)
    (which-Nat n
      j
      (lambda (n-1) (add1 (+ n-1 j))))))
]

Because in Pie, recursive was not an option, the pre-condition was we must have value for each expression, so if there was a recursion, we could get:

@pieblock[
(claim forever (-> Nat Atom))
(define forever
  (lambda (and-ever)
    (forever and-ever)))
]

To avoid it, recursive is not an option.

@section{total function}

A function assigns a value to every possible argument is called a total function. In Pie, all functions are total.

Total function is important that we always want to get a value from application.

@section{why is @pie[rec-Nat] always safe to use}

The reason is because @pie[rec-Nat] guaranteed to reach the base.
Every @pie[Nat] is @pie[zero] or @pie[(add1 n)], where @pie[n] is a smaller @pie[Nat].

@section{elim-Pair to Π}

@pieblock[
(elim-Pair
  A D
  X
  p
  f)
]

where @pie[p] is a @pie[(Pair A D)], and @pie[f] takes values of the expression from @pie[car] and @pie[cdr] from @pie[p].

Candidate type of @pie[elim-Pair]

@pieblock[
(-> A D
  X
  (Pair A D)
  (-> A D
    X)
  X)
]

But this cannot be true. So we need @pie[Π].

Example of @pie[Π]

@pieblock[
(claim flip
  (Π ((A U) (D U))
    (-> (Pair A D)
      (Pair D A))))
(define flip
  (lambda (A D)
    (lambda (p)
      (cons (cdr p) (car p)))))
]

So basically @pie[Π] says there has @pie[A] is @pie[U] and @pie[D] is @pie[U], and the body of @pie[Π] following this truth. This also means lambda-expression's type can be a @pie[U]-expression.

In the expression

@pieblock[
(Π ((A U) (D U)
  (-> (Pair A D)
    (Pair D A))))
]

the body of it was:

@pieblock[
(-> (Pair A D)
  (Pair D A))
]

Now, we know how to @pie[claim] type of @pie[elim-Pair]:

@pieblock[
(claim elim-Pair
  (Π ((A U)
      (D U)
      (X U))
    (-> (Pair A D)
        (-> A D
          X)
      X)))
(define elim-Pair
  (lambda (A D X)
    (lambda (p f) (f (car p) (cdr p)))))
]

@section{more Π}

@bold{Type} can dependent on @bold{Term}, @pie[Vec] is an example:

@itemlist[
  @item{If @pie[E] is a type and @pie[k] is a @pie[Nat], then @pie[(Vec E k)] is a type.}
  @item{@pie[vecnil] is a @pie[(Vec E zero)]}
  @item{If @pie[e] is an @pie[E] and @pie[es] is a @pie[(Vec E k)], @pie[(vec:: e es)] is a @pie[(Vec E (add1 k))]}
]

To get first element from @pie[List] in Pie is impossible. Because we cannot sure that's safe, @pie[nil] has no entry!
But get first element from @pie[(Vec E (add1 k))] is possible for every @pie[k] is @pie[Nat].

To define such function, it's easy to guess @pie[Π] also takes @bold{Term} as parameter.

Here we go(with eliminator @pie[head] and @pie[tail]):

@pieblock[
(claim first
  (Π ((E U)
      (l Nat))
    (-> (Vec E (add1 l))
      E)))
(define first
  (lambda (E l)
    (lambda (es)
      (head es))))
]

and @pie[->]-expressions is a shorter way of writing @pie[Π]-expressions when argument name is not used in the @pie[Π]-expression's body. Therefore, the following definition is the same as above:

@pieblock[
(claim first
  (Π ((E U)
      (l Nat))
    (Π ((es (Vec E (add1 l))))
      E)))
]
