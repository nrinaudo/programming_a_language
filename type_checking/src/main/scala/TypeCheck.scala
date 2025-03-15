package type_checking

import Expr.*

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class TypeEnv private (env: List[TypeEnv.Binding]):
  def bind(name: String, tpe: Type) =
    TypeEnv(TypeEnv.Binding(name, tpe) :: env)

  def lookup(name: String) =
    env
      .find(_.name == name)
      .map(_.tpe)
      .toRight(s"Type binding $name not found")

object TypeEnv:
  private case class Binding(name: String, tpe: Type)

  val empty = TypeEnv(List.empty)

// - Type checking -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private def expect(expr: Expr, tpe: Type, Γ: TypeEnv) = typeCheck(expr, Γ).flatMap:
  case `tpe` => Right(())
  case other => Left(s"Expected $tpe but found $other")

private def checkGt(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    _ <- expect(lhs, Type.Num, Γ)
    _ <- expect(rhs, Type.Num, Γ)
  yield Type.Bool

private def checkAdd(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    _ <- expect(lhs, Type.Num, Γ)
    _ <- expect(rhs, Type.Num, Γ)
  yield Type.Num

private def checkCond(pred: Expr, onT: Expr, onF: Expr, Γ: TypeEnv) =
  for
    _ <- expect(pred, Type.Bool, Γ)
    x <- typeCheck(onT, Γ)
    _ <- expect(onF, x, Γ)
  yield x

private def checkLet(name: String, value: Expr, body: Expr, Γ: TypeEnv) =
  for
    x <- typeCheck(value, Γ)
    y <- typeCheck(body, Γ.bind(name, x))
  yield y

private def checkFun(param: String, x: Type, body: Expr, Γ: TypeEnv) =
  for y <- typeCheck(body, Γ.bind(param, x))
  yield x -> y

private def checkApply(fun: Expr, arg: Expr, Γ: TypeEnv) =
  typeCheck(fun, Γ).flatMap:
    case Type.Fun(x, y) => expect(arg, x, Γ).map(_ => y)
    case other          => Left(s"Expected function, found $other")

private def checkLetRec(name: String, value: Expr, x: Type, body: Expr, Γ: TypeEnv) =
  val Γʹ = Γ.bind(name, x)

  for
    _ <- expect(value, x, Γʹ)
    y <- typeCheck(body, Γʹ)
  yield y

private def checkRef(name: String, Γ: TypeEnv) = Γ.lookup(name)

def typeCheck(expr: Expr, Γ: TypeEnv): Either[String, Type] = expr match
  case _: Bool                          => Right(Type.Bool)
  case _: Num                           => Right(Type.Num)
  case Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
  case Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
  case Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
  case Let(name, value, body)           => checkLet(name, value, body, Γ)
  case LetRec(name, value, vType, body) => checkLetRec(name, value, vType, body, Γ)
  case Ref(name)                        => checkRef(name, Γ)
  case Fun(param, pType, body)          => checkFun(param, pType, body, Γ)
  case Apply(fun, arg)                  => checkApply(fun, arg, Γ)
