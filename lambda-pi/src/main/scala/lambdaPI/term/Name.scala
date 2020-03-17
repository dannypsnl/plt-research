package lambdaPI.term

sealed trait Name {
  // `vfree` create value from free variable
  def vfree(): Value = {
    VNeutral(NFree(this))
  }
}
case class Global(string: String) extends Name
case class Local(int: Int) extends Name
case class Quote(int: Int) extends Name