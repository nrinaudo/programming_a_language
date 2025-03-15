package type_inference

import Expr.*

case class InfState(curr: Int, ϕ: Map[Int, TypeInf]):
  def set(i: Int, t: TypeInf) = copy(ϕ = ϕ + (i -> t))
  def get(i: Int)             = ϕ.get(i)

  def getType(t: TypeInf): Either[String, Type] = t match
    case TypeInf.Num  => Right(Type.Num)
    case TypeInf.Bool => Right(Type.Bool)
    case TypeInf.Fun(x, y) =>
      for
        x <- getType(x)
        y <- getType(y)
      yield Type.Fun(x, y)
    case TypeInf.Var(i) =>
      ϕ.get(i) match
        case None     => Left(s"Failed to infer type for variable $$$i")
        case Some(t2) => getType(t2)

object InfState:
  def empty: InfState = InfState(0, Map.empty)
