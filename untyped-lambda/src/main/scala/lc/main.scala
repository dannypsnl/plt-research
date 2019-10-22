import lc.lang._
import lc.interpreter._

object Main extends App {
  val runner = new Runner()
  val result = runner.run(
    Application(
      Lambda("x", Variable("x")),
      LiteralInt(1)
    )
  )
  println(result)
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
