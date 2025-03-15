package type_inference

import Expr.*
import Inferrer.*

// - Type checking result ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Typing(expr: Expr[TypeInf], t: TypeInf)

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class TypeEnv private (env: List[TypeEnv.Binding]):
  def bind(name: String, tpe: TypeInf) =
    TypeEnv(TypeEnv.Binding(name, tpe) :: env)

  def lookup(name: String) =
    env
      .find(_.name == name)
      .map(_.tpe)
      .toRight(s"Type binding $name not found")

object TypeEnv:
  private case class Binding(name: String, tpe: TypeInf)

  val empty = TypeEnv(List.empty)

// - Type unification --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private def occurs(i: Int, x: TypeInf): Inferrer[Boolean] = x match
  case TypeInf.Bool => pure(false)
  case TypeInf.Num  => pure(false)
  case TypeInf.Fun(x, y) =>
    occurs(i, x).flatMap:
      case true  => pure(true)
      case false => occurs(i, y)
  case TypeInf.Var(`i`) => pure(true)
  case TypeInf.Var(j) =>
    get(j).flatMap:
      case Some(y) => occurs(i, y)
      case None    => pure(false)

private def assign(i: Int, x: TypeInf) =
  occurs(i, x).flatMap:
    case true  => fail(s"Infinite type $x")
    case false => set(i, x)

private def unifyVar(i: Int, x: TypeInf) =
  get(i).flatMap:
    case None    => assign(i, x)
    case Some(y) => unify(x, y)

private def unify(t1: TypeInf, t2: TypeInf): Inferrer[Unit] =
  (t1, t2) match
    case (TypeInf.Num, TypeInf.Num)   => pure(())
    case (TypeInf.Bool, TypeInf.Bool) => pure(())
    case (TypeInf.Fun(x1, y1), TypeInf.Fun(x2, y2)) =>
      for
        _ <- unify(x1, x2)
        _ <- unify(y1, y2)
      yield ()
    case (TypeInf.Var(i), x) => unifyVar(i, x)
    case (x, TypeInf.Var(i)) => unifyVar(i, x)
    case _                   => fail(s"Failed to unify $t1 and $t2")

// - Type inference ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

def checkGt(lhs: Expr[Option[Type]], rhs: Expr[Option[Type]], Γ: TypeEnv) =
  for
    lhs <- expect(lhs, TypeInf.Num, Γ)
    rhs <- expect(rhs, TypeInf.Num, Γ)
  yield Typing(Gt(lhs, rhs), TypeInf.Bool)

def checkAdd(lhs: Expr[Option[Type]], rhs: Expr[Option[Type]], Γ: TypeEnv) =
  for
    lhs <- expect(lhs, TypeInf.Num, Γ)
    rhs <- expect(rhs, TypeInf.Num, Γ)
  yield Typing(Add(lhs, rhs), TypeInf.Num)

def checkCond(pred: Expr[Option[Type]], onT: Expr[Option[Type]], onF: Expr[Option[Type]], Γ: TypeEnv) =
  for
    pred           <- expect(pred, TypeInf.Bool, Γ)
    Typing(onT, x) <- typeCheck(onT, Γ)
    onF            <- expect(onF, x, Γ)
  yield Typing(Cond(pred, onT, onF), x)

def checkLet(name: String, value: Expr[Option[Type]], x: TypeInf, body: Expr[Option[Type]], Γ: TypeEnv) =
  for
    value           <- expect(value, x, Γ)
    Typing(body, y) <- typeCheck(body, Γ.bind(name, x))
  yield Typing(Let(name, value, x, body), y)

def checkFun(
    param: String,
    x: TypeInf,
    body: Expr[Option[Type]],
    Γ: TypeEnv
) =
  for Typing(body, y) <- typeCheck(body, Γ.bind(param, x))
  yield Typing(Fun(param, x, body), x -> y)

def checkApply(fun: Expr[Option[Type]], arg: Expr[Option[Type]], Γ: TypeEnv) =
  for
    Typing(arg, x) <- typeCheck(arg, Γ)
    y              <- freshVar
    fun            <- expect(fun, x -> y, Γ)
  yield Typing(Apply(fun, arg), y)

def checkLetRec(name: String, value: Expr[Option[Type]], x: TypeInf, body: Expr[Option[Type]], Γ: TypeEnv) =
  val Γʹ = Γ.bind(name, x)
  for
    value           <- expect(value, x, Γʹ)
    Typing(body, y) <- typeCheck(body, Γʹ)
  yield Typing(LetRec(name, value, x, body), y)

def checkRef(name: String, Γ: TypeEnv) = from:
  for t <- Γ.lookup(name)
  yield Typing(Ref(name), t)

def expect(expr: Expr[Option[Type]], t: TypeInf, Γ: TypeEnv) =
  for
    Typing(expr, t2) <- typeCheck(expr, Γ)
    _                <- unify(t, t2)
  yield expr

def withType[A](t: Option[Type])(f: TypeInf => Inferrer[A]): Inferrer[A] =
  t match
    case Some(t) => f(TypeInf.from(t))
    case None    => freshVar.flatMap(f)

def typeCheck(expr: Expr[Option[Type]], Γ: TypeEnv): Inferrer[Typing] =
  expr match
    case Bool(value)                      => pure(Typing(Bool(value), TypeInf.Bool))
    case Num(value)                       => pure(Typing(Num(value), TypeInf.Num))
    case Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
    case Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
    case Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
    case Let(name, value, vType, body)    => withType(vType)(checkLet(name, value, _, body, Γ))
    case LetRec(name, value, vType, body) => withType(vType)(checkLetRec(name, value, _, body, Γ))
    case Ref(name)                        => checkRef(name, Γ)
    case Fun(param, pType, body)          => withType(pType)(checkFun(param, _, body, Γ))
    case Apply(fun, arg)                  => checkApply(fun, arg, Γ)
