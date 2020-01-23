package stlc

object main extends App {
  val result = TypeChecker.check(
    Application(
      Lambda("x", Primitive("bool"),
        If(Variable("x"), LitInt(1), LitInt(2))),
      LitBool(true)
    )
  )
  println(result)
}
