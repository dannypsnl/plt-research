package hm

sealed class Type {
  def occurs(t: Type): Boolean = {
    t match {
      case Primitive(_) => false
      case Arrow(t1, t2) => this.occurs(t1) || this.occurs(t2)
      case TypeVariable(_) => this == t
    }
  }
}
// int | bool | string
case class Primitive(name: String) extends Type {
  override def toString: String = name
}
// * -> *
case class Arrow(arg: Type, ret: Type) extends Type {
  override def toString: String = arg.toString ++ " -> " ++ ret.toString
}
// 'a
case class TypeVariable(name: String) extends Type {
  override def toString: String = "'" ++ name
}
