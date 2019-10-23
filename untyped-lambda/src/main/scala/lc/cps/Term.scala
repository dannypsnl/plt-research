package lc.cps

sealed trait ATerm
case class AVar(name: String) extends ATerm {
  override def toString: String = name
}
case class ALambda(params: List[String], cexpr: CTerm) extends ATerm {
  override def toString: String = "(lambda (" ++ params.mkString(" ") ++ ") " ++ cexpr.toString ++ ")"
}

sealed trait CTerm
case class CApplication(func: ATerm, args: List[ATerm]) extends CTerm {
  override def toString: String = "(" ++ func.toString ++ " " ++ args.mkString(" ") ++ ")"
}