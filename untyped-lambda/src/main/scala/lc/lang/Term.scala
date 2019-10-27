package lc.lang

sealed class Term
// Atomic expression
// var
case class Variable(name: String) extends Term
// func arg
case class Application(func: Term, arg: Term) extends Term
case class MultipleLambda(params: List[String], body: Term) extends Term
// \param.body
case class Lambda(param: String, body: Term) extends Term
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
case class MultipleApplication(func: Term, args: List[Term]) extends Term
