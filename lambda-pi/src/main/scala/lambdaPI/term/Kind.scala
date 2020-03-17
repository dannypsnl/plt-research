package lambdaPI.term

sealed trait Kind {}
case class Star() extends Kind

sealed trait Info {}
case class HasKind(kind: Kind) extends Info
case class HasType(ty: Type) extends Info
