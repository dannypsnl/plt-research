#lang scribble/manual

@require[scribble-math]

@title{Simply typed lambda calculus(STLC)}

@section{formula}

@${\vdash} means @bold{in context}, so @${x : \mathbb{N} \vdash x + 3 : \mathbb{N}} read as
@bold{in context @${x : \mathbb{N}}, can judge @${x + 3 : \mathbb{N}}}, important thing was we can have several context separate by `,`,
so @${a, b \vdash c} means @bold{in context a and b, can judge c}

@${\frac{a}{b}} means @${\frac{premise}{conculsion}}, and no premise was ok, e.g. @${\frac{}{b}} represents @${b} is true anyway

@${\Gamma} represents context, @${\Gamma(x)} represents lookup @${x} in @${\Gamma}.
@${\Gamma} means a set of type decalrations @${x_1 : A, x_2 : B, x_3 : C ...}

@$${
	\frac{
	\Gamma (x) = A
	}{
	\Gamma \vdash x : A
	} Var
}

@$${
	\frac{
	\Gamma , x : A \vdash E : B
	}{
	\Gamma \vdash (\lambda x^{A} . E) : (A \rightarrow B)
	} Abs
}

@$${
	\frac{
	\Gamma \vdash E_{1} : (A \rightarrow B) \Gamma \vdash E_{2} : A
	}{
	\Gamma \vdash (E_{1} E_{2}) : B
	} App
}

