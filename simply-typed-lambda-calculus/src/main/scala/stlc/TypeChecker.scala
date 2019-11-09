package stlc

import scala.collection.mutable.HashMap

object TypeChecker {
  var env = new Env(None)
  def check(term: Term): Type = {
    term match {
      case LitInt(_) => Primitive("int")
      case LitBool(_) => Primitive("bool")
      case Variable(name) => env.get(name)
      case Lambda(param, typeOfParam, body) => {
        env.set(param, typeOfParam)
        // now we get a new env
        val typeOfBody = check(body)
        Arrow(typeOfParam, typeOfBody)
      }
      case Application(f, arg) => {
        val typeOfFunc = check(f)
        val typeOfArg = check(arg)
        typeOfFunc match {
          case Arrow(t1, t2) => {
            if (typeOfArg == t1) {
              t2
            } else {
              throw new TypeMismatchException(t1, typeOfArg)
            }
          }
          case _ => throw new NotFunctionException()
        }
      }
      case If(cond, thenE, elseE) => {
        val typeOfCond = check(cond)
        if (typeOfCond == Primitive("bool")) {
          val thenT = check(thenE)
          val elseT = check(elseE)
          if (thenT == elseT) {
            thenT
          } else {
            throw new TypeMismatchException(thenT, elseT)
          }
        } else {
          throw new TypeMismatchException(Primitive("bool"), typeOfCond)
        }
      }
    }
  }
}

class TypeMismatchException(expected: Type, actual: Type) extends Exception {}
class NotFunctionException() extends  Exception {}

class Env(parent: Option[Env]) {
  var internalMap = new HashMap[String, Type]()
  def get(name: String) : Type = {
    val r = internalMap.get(name)
    if (r.isEmpty && parent.isDefined) {
      parent.get.get(name)
    } else if (r.isDefined) {
      r.get
    } else {
      throw new NotFoundException(name)
    }
  }
  def set(name: String, value: Type) = {
    internalMap.put(name, value)
    ()
  }
}

class NotFoundException(name: String) extends Exception {}
