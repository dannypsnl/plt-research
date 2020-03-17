package lambdaPI

import scala.collection.mutable
import lambdaPI.term._

class Context(var ctx: mutable.Map[Name, Value]) {
  type Type = Value
  type Result[A] = Either[A, String]

  def typeInferableZero: InferableTerm => Result[Type] = typeInferable(0, _)

  def typeInferable(i: Int, term: InferableTerm): Result[Type] = {
    term match {
      case Ann(e, p) =>
        typeCheckable(i, p, VStar()) match {
          case Right(v) => return Right(v)
          case _        => ()
        }
        val ty = evalCheckable(p, List.empty)
        typeCheckable(i, e, ty) match {
          case Right(v) => Right(v)
          case _        => Left(ty)
        }
      case Star() => Left(VStar())
      case Pi(p, pp) =>
        typeCheckable(i, p, VStar()) match {
          case Right(v) => Right(v)
          case _ =>
            val sr = evalCheckable(p, List.empty)
            ctx += (Local(i) -> sr)
            typeCheckable(i + 1, substCheckable(0, Free(Local(i)), pp), VStar())
            Left(VStar())
        }
      case Free(name) =>
        ctx.get(name) match {
          case Some(ty) => Left(ty)
          case _        => Right("unknown identifier")
        }
      case Application(e, ep) =>
        typeInferable(i, e) match {
          case Right(value) => Right(value)
          case Left(value) =>
            value match {
              case VPi(t, tp) =>
                // check argument type matched
                typeCheckable(i, ep, t) match {
                  case Right(v) => return Right(v)
                  case _        => ()
                }
                Left(tp(evalCheckable(ep, List.empty)))
              case _ => Right("illegal application")
            }
        }
      case _ => Right("unreachable")
    }
  }

  def typeCheckable(i: Int, term: CheckableTerm, ty: Type): Result[Unit] = {
    (term, ty) match {
      case (Inf(e), v) =>
        typeInferable(i, e) match {
          case Left(vp) =>
            if (vp.quoteZero != v.quoteZero)
              Right("type mismatch, want: %s but got: %s".format(vp, v))
            else Left(())
          case Right(value) => Right(value)
        }
      case (Lam(checkableTerm), VPi(s, sp)) =>
        ctx += (Local(i) -> s)
        typeCheckable(i + 1,
                      substCheckable(0, Free(Local(i)), checkableTerm),
                      sp(Local(i).vfree()))
      case _ => Right("type mismatch")
    }
  }

  def substInferable(i: Int,
                     r: InferableTerm,
                     t: InferableTerm): InferableTerm = {
    t match {
      case Ann(checkableE, ty) =>
        Ann(substCheckable(i, r, checkableE), substCheckable(i, r, ty))
      case Bound(j)   => if (i == j) r else Bound(j)
      case Free(name) => Free(name)
      case Application(inferableTerm, checkableTerm) =>
        substInferable(
          i,
          r,
          Application(inferableTerm, substCheckable(i, r, checkableTerm)))
      case Star() => Star()
      case Pi(t1, t2) =>
        Pi(
          substCheckable(i, r, t1),
          substCheckable(i + 1, r, t2),
        )
    }
  }
  def substCheckable(i: Int,
                     r: InferableTerm,
                     t: CheckableTerm): CheckableTerm = {
    t match {
      case Inf(inferableTerm) => Inf(substInferable(i, r, inferableTerm))
      case Lam(checkableTerm) =>
        Lam(substCheckable(i + 1, r, checkableTerm))
    }
  }

  // eval
  def evalInferable(t: InferableTerm, v: List[Value]): Value = {
    t match {
      case Ann(checkableTerm, _) => evalCheckable(checkableTerm, v)
      case Bound(i) =>
        v(i) // NOTE: scala list indexing look just like function call
      case Free(name) => name.vfree()
      case Application(e, ep) =>
        VApp.vapp(evalInferable(e, v), evalCheckable(ep, v))
      case Star() => VStar()
      case Pi(t1, t2) =>
        VPi(
          evalCheckable(t1, v),
          x => {
            evalCheckable(t2, x :: v)
          }
        )
    }
  }
  def evalCheckable(t: CheckableTerm, v: List[Value]): Value = {
    t match {
      case Inf(inferableTerm) => evalInferable(inferableTerm, v)
      case Lam(checkableTerm) => VLam(x => evalCheckable(checkableTerm, x :: v))
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
