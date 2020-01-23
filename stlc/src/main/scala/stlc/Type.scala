package stlc

sealed class Type
// int | bool
case class Primitive(name: String) extends Type
// * -> *
case class Arrow(arg: Type, ret: Type) extends Type
