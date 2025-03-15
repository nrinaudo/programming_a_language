package type_inference

import Expr.*

// - Inference state ---------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class InfState:
  var currentVar = 0
  val ϕ          = collection.mutable.Map.empty[Int, TypeInf]

  def toInf(tpe: Option[Type]) =
    tpe
      .map(TypeInf.from)
      .getOrElse(freshVar)

  def freshVar: TypeInf =
    val v = TypeInf.Var(currentVar)
    currentVar += 1
    v

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

  def occurs(i: Int, x: TypeInf): Boolean = x match
    case TypeInf.Bool      => false
    case TypeInf.Num       => false
    case TypeInf.Fun(x, y) => occurs(i, x) || occurs(i, y)
    case TypeInf.Var(`i`)  => true
    case TypeInf.Var(j)    => ϕ.get(j).map(occurs(i, _)).getOrElse(false)

  def assign(i: Int, x: TypeInf) =
    if occurs(i, x) then Left(s"Infinite type $x")
    else
      ϕ(i) = x
      Right(())

  def unifyVar(i: Int, x: TypeInf) =
    ϕ.get(i) match
      case None    => assign(i, x)
      case Some(y) => unify(x, y)

  def unify(t1: TypeInf, t2: TypeInf): Either[String, Unit] =
    (t1, t2) match
      case (TypeInf.Num, TypeInf.Num)   => Right(())
      case (TypeInf.Bool, TypeInf.Bool) => Right(())
      case (TypeInf.Fun(x1, y1), TypeInf.Fun(x2, y2)) =>
        for
          _ <- unify(x1, x2)
          _ <- unify(y1, y2)
        yield ()
      case (TypeInf.Var(i), x) => unifyVar(i, x)
      case (x, TypeInf.Var(i)) => unifyVar(i, x)
      case _                   => Left(s"Failed to unify $t1 and $t2")
