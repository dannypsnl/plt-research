import scala.collection.mutable.HashMap

sealed class Term
// var
case class Variable(name: String) extends Term
// func arg
case class Application(func: Term, arg: Term) extends Term
// \param.body
case class Lambda(param: String, body: Term) extends Term
// 1, 2, 3
case class LiteralInt(i: Int) extends Term

object Main extends App {
  var env = new Env(None)
  val runner = new Runner()
  val result = runner.eval(env,
    Application(
      Lambda("x", Variable("x")),
      LiteralInt(1)
    )
  )
  println(result)
}

class Env(parent: Option[Env]) {
  var internalMap = new HashMap[String, Value]()
  def get(name: String) : Value = {
    val r = internalMap.get(name)
    if (r.isEmpty && parent.isDefined) {
      parent.get.get(name)
    } else if (r.isDefined) {
      r.get
    } else {
      throw new NotFoundException(name)
    }
  }
  def set(name: String, value: Value) = {
    internalMap.put(name, value)
    ()
  }
}

sealed class Value
case class VInt(i: Int) extends Value
case class VClosure(param: String, body: Term, env: Env) extends Value

class Runner {
  def eval(env: Env, term: Term): Value = {
    term match {
      case LiteralInt(i) => VInt(i)
      case Variable(name) => env.get(name)
      case Application(func, arg) =>{
        val VClosure(param, body, lambdaEnv) = eval(env, func)
        val v = eval(env, arg)
        lambdaEnv.set(param, v)
        eval(lambdaEnv, body)
      }
      case Lambda(param, body) => VClosure(param, body, new Env(Some(env)))
    }
  }
}

class NotFoundException(name: String) extends Exception {}