package lc.lang

sealed class Term
// var
case class Variable(name: String) extends Term
// func arg
case class Application(func: Term, arg: Term) extends Term
// \param.body
case class Lambda(param: String, body: Term) extends Term
// 1, 2, 3
case class LiteralInt(i: Int) extends Term
