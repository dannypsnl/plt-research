package hm

sealed class Term
case class Let(varName: String, varExpr: Term, body: Term) extends Term
case class Variable(name: String) extends Term
case class Application(func: Term, arg: Term) extends Term
case class Lambda(param: String, body: Term) extends Term
case class LitInt(int: Int) extends Term
case class LitBool(boolean: Boolean) extends Term
case class LitString(string: String) extends Term
case class Binary(op: Operator, left: Term, right: Term) extends Term

sealed class Operator
case class Add() extends Operator
case class Sub() extends Operator
case class Mul() extends Operator
case class Div() extends Operator
case class Equal() extends Operator
case class NotEqual() extends Operator
