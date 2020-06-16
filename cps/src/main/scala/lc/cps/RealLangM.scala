package lc.cps

import lc.lang._
import lc.Gensym

object RealLangM {
  val gensym = new Gensym()
  def transform_k(term: Term, cont: AValue => CValue): CValue = {
    if (isAtomicExpr(term)) {
      return cont(m(term))
    }
    term match {
      case If(condition, thenE, elseE) => {
        val rv = gensym.apply("$rv")
        val newCont = ALambda(List(rv), cont(AVar(rv)))
        transform_k(condition, aCond =>
          CIf(aCond, transform_c(thenE, newCont), transform_c(elseE, newCont)))
      }
      case Application(_, _) => {
        val rv = gensym.apply("$rv")
        val newCont = ALambda(List(rv), cont(AVar(rv)))
        transform_c(term, newCont)
      }
    }
  }
  def transform_c(term: Term, continuation: AValue): CValue = {
    if (isAtomicExpr(term)) {
      return CApplication(continuation, List(m(term)))
    }
    term match {
      case If(condition, thenE, elseE) => {
        val cont = gensym.apply("$k")
        CApplication(
          ALambda(List(cont), transform_k(condition, aCond =>
            CIf(aCond, transform_c(thenE, AVar(cont)), transform_c(elseE, AVar(cont))))),
          List(continuation)
        )
      }
      case Application(f, args) => {
        transform_k(f, fs =>
          transform_k_variant(args, es =>
            CApplication(fs, es :+ continuation)))
      }
    }
  }
  def transform_k_variant(terms: List[Term], cont: List[AValue] => CValue): CValue = {
    terms match {
      case List() => cont(List())
      case head :: rest => transform_k(head, hd =>
        transform_k_variant(rest, t1 => cont(hd :: t1)))
    }
  }
  def m(term: Term): AValue = {
    term match {
      case Lambda(params, body) => {
        val cont = gensym.apply("$k")
        ALambda(params :+ cont, transform_c(body, AVar(cont)))
      }
      case Variable(name) => AVar(name)
      case LiteralInt(i) => ALiteralInt(i)
      case CallWithCurrentContinuation() | CallWithEscapeContinuation() =>
        ALambda(List("f", "cc"), CApplication(
          AVar("f"),
          List(ALambda(List("x", "_"), CApplication(AVar("cc"), List(AVar("x")))), AVar("cc"))
        ))
      case _ => throw new NotATermException(term)
    }
  }

  def isAtomicExpr(term: Term): Boolean = {
    term match {
      case Variable(_) => true
      case Lambda(_, _) => true
      case LiteralInt(_) => true
      case CallWithCurrentContinuation() => true
      case CallWithEscapeContinuation() => true
      case _ => false
    }
  }
}
