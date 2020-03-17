package lambdaPI.term

sealed trait InferableTerm {}
case class Ann(checkableTerm: CheckableTerm, ty: Type) extends InferableTerm
case class Bound(int: Int) extends InferableTerm
case class Free(name: Name) extends InferableTerm
case class Application(inferableTerm: InferableTerm,
                       checkableTerm: CheckableTerm)
    extends InferableTerm
