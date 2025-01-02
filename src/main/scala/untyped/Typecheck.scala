package untyped

import untyped.Expr.*

case class TypeEnv(env: List[TypeEnv.Binding]):
  def lookup(name: String) =
    env
      .find(_.name == name)
      .map(_.tpe)
      .toRight(s"Type binding $name not found")

  def bind(name: String, tpe: Type) =
    TypeEnv(TypeEnv.Binding(name, tpe) :: env)

object TypeEnv:
  case class Binding(name: String, tpe: Type)

  val empty = TypeEnv(List.empty)

def expect(expr: Expr, tpe: Type, Γ: TypeEnv) = typecheck(expr, Γ).flatMap:
  case `tpe` => Right(())
  case other => Left(s"Expected $tpe but found $other")

def checkGt(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    _ <- expect(lhs, Type.Num, Γ)
    _ <- expect(rhs, Type.Num, Γ)
  yield Type.Bool

def checkAdd(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    _ <- expect(lhs, Type.Num, Γ)
    _ <- expect(rhs, Type.Num, Γ)
  yield Type.Num

def checkCond(pred: Expr, onT: Expr, onF: Expr, Γ: TypeEnv) =
  for
    _ <- expect(pred, Type.Bool, Γ)
    x <- typecheck(onT, Γ)
    _ <- expect(onF, x, Γ)
  yield x

def checkLet(name: String, value: Expr, body: Expr, Γ: TypeEnv) =
  for
    x <- typecheck(value, Γ)
    y <- typecheck(body, Γ.bind(name, x))
  yield y

def checkFun(param: String, x: Type, body: Expr, Γ: TypeEnv) =
  typecheck(body, Γ.bind(param, x)).map: y =>
    Type.Fun(x, y)

def checkApply(fun: Expr, arg: Expr, Γ: TypeEnv) =
  typecheck(fun, Γ).flatMap:
    case Type.Fun(x, y) => expect(arg, x, Γ).map(_ => y)
    case other          => Left(s"Expected function, found $other")

def checkLetRec(name: String, value: Expr, x: Type, body: Expr, Γ: TypeEnv) =
  val Γʹ = Γ.bind(name, x)

  for
    _ <- expect(value, x, Γʹ)
    y <- typecheck(body, Γʹ)
  yield y

def typecheck(expr: Expr, Γ: TypeEnv): Either[String, Type] = expr match
  case _: Bool                          => Right(Type.Bool)
  case _: Num                           => Right(Type.Num)
  case Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
  case Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
  case Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
  case Let(name, value, body)           => checkLet(name, value, body, Γ)
  case LetRec(name, value, vType, body) => checkLetRec(name, value, vType, body, Γ)
  case Ref(name)                        => Γ.lookup(name)
  case Fun(param, pType, body)          => checkFun(param, pType, body, Γ)
  case Apply(fun, arg)                  => checkApply(fun, arg, Γ)
