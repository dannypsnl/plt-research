package lc.cps

import lc.lang._
import lc.Gensym

object HybridTransformM {
  val gensym = new Gensym()
  def transform_k(term: Term, cont: ATerm => CTerm): CTerm = {
    term match {
      case Lambda(_, _) => cont(m(term))
      case Variable(_) => cont(m(term))
      case Application(f, e) => {
        val rv = gensym.apply("$rv")
        val newCont = ALambda(List(rv), cont(AVar(rv)))
        transform_k(f, fs =>
          transform_k(e, es =>
            CApplication(fs, List(es, newCont))))
      }
    }
  }
  def transform_c(term: Term, cont: ATerm): CTerm = {
    term match {
      case Lambda(_, _) => CApplication(cont, List(m(term)))
      case Variable(_) => CApplication(cont, List(m(term)))
      case Application(f, e) => {
        transform_k(f, fs =>
          transform_k(e, es =>
            CApplication(fs, List(es, cont))))
      }
    }
  }
  def m(term: Term): ATerm = {
    term match {
      case Lambda(param, body) => {
        val cont = gensym.apply("$k")
        ALambda(List(param, cont), transform_c(body, AVar(cont)))
      }
      case Variable(name) => AVar(name)
      case Application(_, _) => throw new NotATermException(term)
    }
  }
}
