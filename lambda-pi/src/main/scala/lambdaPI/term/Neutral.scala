package lambdaPI.term

sealed trait Neutral {}
case class NFree(name: Name) extends Neutral
case class NApp(neutral: Neutral, value: Value) extends Neutral