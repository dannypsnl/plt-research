package lc.lang

sealed class Term
// Atomic expression
// var
case class Variable(name: String) extends Term
case class Lambda(params: List[String], body: Term) extends Term
// 1, 2, 3
case class LiteralInt(i: Int) extends Term
case class CallWithCurrentContinuation() extends Term
case class CallWithEscapeContinuation() extends Term
// Complex expression
// if (condition)
//   then thenE
//   else elseE
case class If(condition: Term, thenE: Term, elseE: Term) extends Term
// func arg...
case class Application(func: Term, args: List[Term]) extends Term
