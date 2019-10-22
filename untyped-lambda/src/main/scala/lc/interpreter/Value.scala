package lc.interpreter

import lc.lang.Term

sealed class Value
case class VInt(i: Int) extends Value
case class VClosure(param: String, body: Term, env: Env) extends Value