package lc.cps

import lc.lang._
import lc.Gensym

object HighOrderM {
  val gensym = new Gensym()
  def m(term: Term): ATerm = {
    term match {
      case Lambda(param, body) => {
        val continuation = gensym.apply("$k")
        val newBody = t(body, rv => CApplication(AVar(continuation), List(rv)))
        // insert continuation
        ALambda(List(param, continuation), newBody)
      }
      case Variable(name) => AVar(name)
      case Application(_, _) => throw new NotATermException(term)
    }
  }
  def t(term: Term, continuation: ATerm => CTerm): CTerm = {
    term match {
      case Lambda(_, _) => continuation(m(term))
      case Variable(_) => continuation(m(term))
      case Application(func, arg) => {
        val f = gensym.apply("$rv")
        val cont = ALambda(List(f), continuation(AVar(f)))
        t(func, (fs: ATerm) =>
          t(arg, es =>
            CApplication(fs, List(es, cont))))
      }
    }
  }
}