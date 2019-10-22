package lc.cps

import lc.lang._
import lc.Gensym

sealed trait ATerm
case class AVar(name: String) extends ATerm
case class ALambda(params: List[String], cexpr: CTerm) extends ATerm

sealed trait CTerm
case class CApplication(cont: ATerm, args: List[ATerm]) extends CTerm

object NaiveM {
  val gensym = new Gensym()
  def nm(term: Term): ATerm = {
    term match {
      case Lambda(param, body) => {
        val continuation = gensym.apply("v")
        val newBody = nt(body, AVar(continuation))
        ALambda(List(param, continuation), newBody)
      }
      case Variable(name) => AVar(name)
      case Application(_, _) => throw new NotATermException(term)
      case LiteralInt(_) => throw new NotATermException(term)
    }
  }
  def nt(term: Term, continuation: ATerm): CTerm = {
    term match {
      case Lambda(_, _) => CApplication(continuation, List(nm(term)))
      case Variable(_) => CApplication(continuation, List(nm(term)))
      case Application(f, arg) => {
        val fs = gensym.apply("v")
        val es = gensym.apply("v")

        val aexpr0 = ALambda(List(es), CApplication(AVar(fs), List(AVar(es), continuation)))
        val cexpr = nt(arg, aexpr0)
        val aexpr1 = ALambda(List(fs), cexpr)
        nt(f, aexpr1)
      }
      case LiteralInt(_) => throw new NotATermException(term)
    }
  }
}

class NotATermException(term: Term) extends Exception {}