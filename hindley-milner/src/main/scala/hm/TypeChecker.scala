package hm

import scala.collection.mutable.HashMap

class TypeChecker {
  var env = new Env(None)
  var unique: Int = 0

  def inference(term: Term): (Type, SubstMap) = {
    term match {
      case LitInt(_) => (Primitive("int"), new SubstMap)
      case LitBool(_) => (Primitive("bool"), new SubstMap)
      case LitString(_) => (Primitive("string"), new SubstMap)
      case Variable(v) => (env.getType(v), new SubstMap)
      case Lambda(param, body) => {
        val freeVar = fresh()
        env.bindType(param, freeVar)
        val (retType, substMap) = inference(body)
        (Arrow(substMap.substType(freeVar), retType), substMap)
      }
      case Application(lambda, argument) => {
        val (arrowType, lambdaSub) = inference(lambda)
        lambdaSub.substEnv(env)
        val (argType, argSub) = inference(argument)
        val freeVar = fresh()
        val subst = unify(argSub.substType(arrowType), Arrow(argType, freeVar))
        (subst.substType(freeVar), subst ++ argSub ++ lambdaSub)
      }
      case Let(varName, varExpr, body) => {
        val (varType, varSub) = inference(varExpr)
        env.bindType(varName, varType)
        val (bodyType, bodySub) = inference(body)
        (bodyType, bodySub ++ varSub)
      }
      case Binary(op, left, right) => {
        val (leftType, sub1) = inference(left)
        sub1.substEnv(env)
        val (rightType, sub2) = inference(right)
        val sub3 = unify(sub2.substType(leftType), opLeftType(op))
        val sub4 = unify(sub3.substType(rightType), opRighttType(op))
        (opType(op), sub4 ++ sub3 ++ sub2 ++ sub1)
      }
    }
  }

  def opType(op: Operator): Type = {
    op match {
      case Add() | Sub() | Mul() | Div() => Primitive("int")
      case Equal() | NotEqual() => Primitive("bool")
    }
  }

  def opLeftType(op: Operator): Type = Primitive("int")
  def opRighttType(op: Operator): Type = Primitive("int")

  def unify(left: Type, right: Type): SubstMap = {
    (left, right) match {
      case (Primitive("int"), Primitive("int")) => new SubstMap
      case (Primitive("bool"), Primitive("bool")) => new SubstMap
      case (Primitive("string"), Primitive("string")) => new SubstMap
      case (Arrow(t1, t2), Arrow(t1p, t2p)) => {
        val sub1 = unify(t1, t1p)
        val sub2 = unify(sub1.substType(t2), sub1.substType(t2p))
        sub1 ++ sub2
      }
      case (v@TypeVariable(_), t) => unify(t, v)
      case (t, v@TypeVariable(_)) => {
        val sub = new SubstMap
        if (t == v || !v.occurs(t)) {
          sub.addSubst(v, t)
        }
        sub
      }
      case (t1, t2) => throw new UnifyException(t1, t2)
    }
  }

  def fresh() : Type = {
    val range: Array[String] = ('a' to 'z').map(c => c.toString).toArray
    unique += 1
    TypeVariable(range.apply(unique - 1))
  }
}

class UnifyException(t1: Type, t2: Type) extends Exception {
  override def getMessage: String = "can't unify type: " ++ t1.toString ++ " and " ++ t2.toString
}
class TypeMismatchException(expected: Type, actual: Type) extends Exception {
  override def getMessage: String = "expected type: " ++ expected.toString ++ ", but got: " ++ actual.toString
}
class NotFunctionException() extends  Exception {}

class SubstMap() {
  var m: List[Type => Type] = List()
  def ++(rhs: SubstMap): SubstMap = {
    this.m = this.m ++ rhs.m
    this
  }
  def subst(t: Type): Type = {
    t match {
      // int | bool | string
      case Primitive(typ) => Primitive(typ)
      case Arrow(t1, t2) => Arrow(subst(t1), subst(t2))
      case TypeVariable(_) => substType(t)
    }
  }
  def addSubst(fromType: Type, toType: Type) = {
    m :+ ((t: Type) => if (t == fromType) toType else fromType)
  }
  def substType(fromType: Type): Type = {
    m.foldRight(fromType)((sub, t) => sub(t))
  }
  def substEnv(env: Env) = {
    env.internalMap.foreach[Type](typ => substType(typ._2))
  }
}

class Env(parent: Option[Env]) {
  var internalMap = new HashMap[String, Type]()
  def getType(name: String) : Type = {
    val r = internalMap.get(name)
    if (r.isEmpty && parent.isDefined) {
      parent.get.getType(name)
    } else if (r.isDefined) {
      r.get
    } else {
      throw new NotFoundException(name)
    }
  }
  def bindType(name: String, value: Type) = {
    internalMap.put(name, value)
    ()
  }
}

class NotFoundException(name: String) extends Exception {}
