package lambdaPI.term

sealed trait InferableTerm {}
case class Ann(c1: CheckableTerm, c2: CheckableTerm) extends InferableTerm
case class Star() extends InferableTerm
case class Pi(t1: CheckableTerm, t2: CheckableTerm) extends InferableTerm
case class Bound(int: Int) extends InferableTerm
case class Free(name: Name) extends InferableTerm
case class Application(f: InferableTerm, arg: CheckableTerm)
    extends InferableTerm
