package lc.cps

sealed trait ATerm
case class AVar(name: String) extends ATerm {
  override def toString: String = name
}
case class ALambda(params: List[String], cexpr: CTerm) extends ATerm {
  override def toString: String = "(lambda (" ++ params.mkString(" ") ++ ") " ++ cexpr.toString ++ ")"
}
case class ALiteralInt(i: Int) extends ATerm {
  override def toString: String = i.toString
}

sealed trait CTerm
case class CApplication(func: ATerm, args: List[ATerm]) extends CTerm {
  override def toString: String = "(" ++ func.toString ++ " " ++ args.mkString(" ") ++ ")"
}
case class CIf(condition: ATerm, thenE: CTerm, elseE: CTerm) extends CTerm {
  override def toString: String = "if (" ++ condition.toString ++ ") then {\n  " ++
    thenE.toString ++ "} else {\n  " ++
    elseE.toString ++ "}"
}