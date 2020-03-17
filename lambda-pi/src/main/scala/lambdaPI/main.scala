package lambdaPI

import lambdaPI.term._

import scala.collection.mutable

object main extends App {
  val id = Lam(Inf(Bound(0)))
  val const = Lam(Lam(Inf(Bound(1))))
  val term1 = Application(Ann(id, Fun(tfree("a"), tfree("a"))), free("y"))

  val interpreter = new Env(List())
  val env1 = new Context(
    mutable.Map(
      Global("y") -> HasType(tfree("a")),
      Global("a") -> HasKind(Star())
    ))

  println("> eval-inferable(term1).qoute0",
          interpreter.evalInferable(term1).quoteZero)
  // output: Inf(Free(Global(y)))
  println("> type-inferable-0(term1)", env1.typeInferableZero(term1))
  // output: Left(TFree(Global(a)))

  def tfree(a: String): Type = TFree(Global(a))
  def free(x: String): CheckableTerm = Inf(Free(Global(x)))
}
