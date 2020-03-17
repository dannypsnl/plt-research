package lambdaPI

import lambdaPI.term._

class Env(var v: List[Value]) {
  def evalInferable(t: InferableTerm): Value = {
    t match {
      case Ann(checkableTerm, _) => this.evalCheckable(checkableTerm)
      case Bound(i) =>
        this.v(i) // NOTE: scala list indexing look just like function call
      case Free(name) => name.vfree()
      case Application(e, ep) =>
        VApp.vapp(this.evalInferable(e), this.evalCheckable(ep))
    }
  }
  def evalCheckable(t: CheckableTerm): Value = {
    t match {
      case Inf(inferableTerm) => this.evalInferable(inferableTerm)
      case Lam(checkableTerm) => {
        VLam(x => {
          this.v = x :: this.v
          this.evalCheckable(checkableTerm)
        })
      }
    }
  }
}

object VApp {
  def vapp(t: Value, v: Value): Value = {
    t match {
      case VLam(f)           => f(v)
      case VNeutral(neutral) => VNeutral(NApp(neutral, v))
    }
  }
}
