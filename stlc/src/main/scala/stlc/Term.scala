package stlc

sealed class Term
case class Variable(name: String) extends Term
case class Application(func: Term, arg: Term) extends Term
case class Lambda(param: String, paramT: Type, body: Term) extends Term
case class LitInt(n: Int) extends Term
case class LitBool(boolean: Boolean) extends Term
case class If(cond: Term, thenE: Term, elseE: Term) extends Term
