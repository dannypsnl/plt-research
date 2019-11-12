package hm

object main extends App {
  val typeChecker = new TypeChecker
  val (typ, _) = typeChecker.inference(Lambda("x", Variable("x")))
  println(typ)
}
