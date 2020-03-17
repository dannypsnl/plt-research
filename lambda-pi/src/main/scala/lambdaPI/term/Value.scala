package lambdaPI.term

sealed trait Value {
  def quoteZero = quote(0)
  def quote(i: Int): CheckableTerm = {
    this match {
      case VLam(f)     => Lam(f(Quote(i).vfree()).quote(i + 1))
      case VNeutral(n) => Inf(neutralQuote(i, n))
    }
  }
  def neutralQuote(i: Int, neutral: Neutral): InferableTerm = {
    neutral match {
      case NFree(name) => boundFree(i, name)
      case NApp(n, v) =>
        Application(neutralQuote(i, n), v.quote(i))
    }
  }
  def boundFree(i: Int, name: Name): InferableTerm = {
    name match {
      case Quote(k) => Bound(i - k - 1)
      case x        => Free(x)
    }
  }
}
case class VLam(f: Value => Value) extends Value
case class VNeutral(neutral: Neutral) extends Value
