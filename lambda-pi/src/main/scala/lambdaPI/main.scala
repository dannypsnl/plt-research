package lambdaPI

import lambdaPI.term._
import scala.collection.mutable

object main extends App {
  type Type = Value

  val id = Lam(Inf(Bound(0)))
  val const = Lam(Lam(Inf(Bound(1))))
  // type of id is `Pi a:*.a -> a`
  val term1 = Application(Ann(id, Inf(Pi(tfree("a"), tfree("a")))), free("y"))

  val env1 = new Context(
    mutable.Map(
      Global("y") -> VNeutral(NFree(Global("a"))),
      Global("a") -> VStar()
    ))

  println("> eval-inferable(term1).qoute0",
          env1.evalInferable(term1, List.empty).quoteZero)
  // output: Inf(Free(Global(y)))
  println("> type-inferable-0(term1)", env1.typeInferableZero(term1))
  // output: Left(VNeutral(NFree(Global(a))))

  def tfree(a: String): CheckableTerm = Inf(Free(Global(a)))
  def free(x: String): CheckableTerm = Inf(Free(Global(x)))
}
