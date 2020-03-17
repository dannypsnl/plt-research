package lambdaPI.term

sealed trait Type {}
case class TFree(name: Name) extends Type
case class Fun(t1: Type , t2: Type) extends Type