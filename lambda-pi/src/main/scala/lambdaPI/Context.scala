package lambdaPI

import scala.collection.mutable
import lambdaPI.term._

class Context(var ctx: mutable.Map[Name, Info]) {
  type Result[A] = Either[A, String]

  def kindCheckable(ty: Type, k: Kind): Result[Unit] = {
    (ty, k) match {
      case (TFree(name), Star()) =>
        ctx.get(name) match {
          case Some(HasKind(Star())) => Left()
          case _                     => Right("unknown identifier")
        }
      case (Fun(t1, t2), Star()) => {
        kindCheckable(t1, k) match {
          case Right(value) => return Right(value)
          case _            => ()
        }
        kindCheckable(t2, k)
      }
    }
  }

  def typeInferableZero = typeInferable(0, _)

  def typeInferable(i: Int, term: InferableTerm): Result[Type] = {
    term match {
      case Ann(checkableTerm, ty) => {
        kindCheckable(ty, Star()) match {
          case Right(v) => return Right(v)
          case _        => ()
        }
        typeCheckable(i, checkableTerm, ty) match {
          case Right(v) => return Right(v)
          case _        => ()
        }
        Left(ty)
      }
      case Free(name) => {
        ctx.get(name) match {
          case Some(HasType(ty)) => Left(ty)
          case _                 => Right("unknown identifier")
        }
      }
      case Application(inferableTerm, checkableTerm) => {
        typeInferable(i, inferableTerm) match {
          case Right(value) => Right(value)
          case Left(value) => {
            value match {
              case Fun(t1, t2) => {
                // check argument type matched
                typeCheckable(i, checkableTerm, t1) match {
                  case Right(v) => return Right(v)
                  case _        => ()
                }
                Left(t2)
              }
              case _ => Right("illegal application")
            }
          }
        }
      }
      case _ => Right("unreachable")
    }
  }

  def typeCheckable(i: Int, term: CheckableTerm, ty: Type): Result[Type] = {
    (term, ty) match {
      case (Inf(inferableTerm), expectedTy) => {
        typeInferable(i, inferableTerm) match {
          case Left(actualType) =>
            if (actualType != expectedTy) Right("type mismatch")
            else Left(actualType)
          case Right(value) => Right(value)
        }
      }
      case (Lam(checkableTerm), Fun(t1, t2)) => {
        ctx += (Local(i) -> HasType(t1))
        typeCheckable(i + 1,
                      substCheckable(0, Free(Local(i)), checkableTerm),
                      t2)
      }
      case _ => Right("type mismatch")
    }
  }

  def substInferable(i: Int,
                     r: InferableTerm,
                     t: InferableTerm): InferableTerm = {
    t match {
      case Ann(checkableTerm, ty) =>
        Ann(substCheckable(i, r, checkableTerm), ty)
      case Bound(j)   => if (i == j) r else Bound(j)
      case Free(name) => Free(name)
      case Application(inferableTerm, checkableTerm) =>
        substInferable(
          i,
          r,
          Application(inferableTerm, substCheckable(i, r, checkableTerm)))
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
}
