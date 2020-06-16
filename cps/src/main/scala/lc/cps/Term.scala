package lc.cps

sealed trait AValue
case class AVar(name: String) extends AValue {
  override def toString: String = name
}
case class ALambda(params: List[String], cexpr: CValue) extends AValue {
  override def toString: String = "(lambda (" ++ params.mkString(" ") ++ ") " ++ cexpr.toString ++ ")"
}
case class ALiteralInt(i: Int) extends AValue {
  override def toString: String = i.toString
}

sealed trait CValue
case class CApplication(func: AValue, args: List[AValue]) extends CValue {
  override def toString: String = "(" ++ func.toString ++ " " ++ args.mkString(" ") ++ ")"
}
case class CIf(condition: AValue, thenE: CValue, elseE: CValue) extends CValue {
  override def toString: String = "if (" ++ condition.toString ++ ") then {\n  " ++
    thenE.toString ++ "} else {\n  " ++
    elseE.toString ++ "}"
}