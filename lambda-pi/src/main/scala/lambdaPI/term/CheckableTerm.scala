package lambdaPI.term

sealed trait CheckableTerm {}
case class Inf(inferableTerm: InferableTerm) extends CheckableTerm
case class Lam(checkableTerm: CheckableTerm) extends CheckableTerm
