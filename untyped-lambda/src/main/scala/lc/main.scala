import lc.cps._
import lc.lang._
import lc.interpreter._

object Main extends App {
  val runner = new Runner()
  println(runner.run(
    Application(
      Lambda("x", Variable("x")),
      LiteralInt(1)
    )
  ))

  // From: (g a)
  val test = Application(Variable("g"), Variable("a"))
  // with continuation: 'halt
  println(NaiveM.nt(test, AVar("halt")))
  // ((lambda ($f1)
  //   ((lambda ($e1)
  //     ($f1 $e1 halt)) a)) g)

  // with high order continuation: (λ (ans) `(halt ,ans))
  println(HighOrderM.t(test, ans => CApplication(AVar("halt"), List(ans))))
  // (g a (lambda ($rv1) (halt $rv1)))

  // with continuation: 'halt
  println(HybridTransformM.transform_c(test, AVar("halt")))
  // (g a halt)
}

class Runner {
  var env = new Env(None)
  def run(term: Term): Value = {
    eval(env, term)
  }
  def eval(env: Env, term: Term): Value = {
    term match {
      case LiteralInt(i) => VInt(i)
      case Variable(name) => env.get(name)
      case Application(func, arg) => {
        val VClosure(param, body, lambdaEnv) = eval(env, func)
        val v = eval(env, arg)
        lambdaEnv.set(param, v)
        eval(lambdaEnv, body)
      }
      case Lambda(param, body) => VClosure(param, body, new Env(Some(env)))
    }
  }
}
