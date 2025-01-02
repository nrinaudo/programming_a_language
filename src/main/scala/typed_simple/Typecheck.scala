package typed_simple

import untyped.{Expr, Type}
import typed_simple.TypedExpr.*

// - Type checking results ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Typing[A <: Type](expr: TypedExpr[A], repr: TypeRepr[A]):
  def cast[B <: Type](to: TypeRepr[B]): Either[String, TypedExpr[B]] = repr.cast(expr, to)

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class TypeEnv private (env: List[TypeEnv.Binding]):
  def bind[A <: Type](name: String, value: TypeRepr[A]) = TypeEnv(
    TypeEnv.Binding(name, value) :: env
  )
  def lookup(name: String): Either[String, TypeRepr[?]] =
    env.find(_.name == name).map(_.value).toRight(s"Binding $name not found")

object TypeEnv:
  private case class Binding(name: String, value: TypeRepr[?])
  val empty = TypeEnv(List.empty)

// - Actual type checking ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Attempts to type check `expr` as a `TypedExpr[A]`.
private def expect[A <: Type](expr: Expr, expected: TypeRepr[A], Γ: TypeEnv): Either[String, Typing[A]] =
  typecheck(expr, Γ).flatMap:
    case Typing(expr, `expected`) => Right(Typing(expr, expected))
    case Typing(_, other)         => Left(s"Expected $expected but found $other")

private def checkAdd(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    lhs <- expect(lhs, TypeRepr.Num, Γ)
    rhs <- expect(rhs, TypeRepr.Num, Γ)
  yield Typing(Add(lhs.expr, rhs.expr), TypeRepr.Num)

private def checkGt(lhs: Expr, rhs: Expr, Γ: TypeEnv) =
  for
    lhs <- expect(lhs, TypeRepr.Num, Γ)
    rhs <- expect(rhs, TypeRepr.Num, Γ)
  yield Typing(Gt(lhs.expr, rhs.expr), TypeRepr.Bool)

private def checkRef(name: String, Γ: TypeEnv) =
  Γ.lookup(name).map(repr => Typing(Ref(Variable(name, repr)), repr))

private def checkCond(pred: Expr, onT: Expr, onF: Expr, Γ: TypeEnv) =
  for
    pred <- expect(pred, TypeRepr.Bool, Γ)
    onT  <- typecheck(onT, Γ)
    onF  <- expect(onF, onT.repr, Γ)
  yield Typing(Cond(pred.expr, onT.expr, onF.expr), onT.repr)

private def checkLet(name: String, value: Expr, body: Expr, Γ: TypeEnv) =
  for
    value <- typecheck(value, Γ)
    body  <- typecheck(body, Γ.bind(name, value.repr))
  yield Typing(Let(Variable(name, value.repr), value.expr, body.expr), body.repr)

private def checkLetRec(name: String, value: Expr, vType: Type, body: Expr, Γ: TypeEnv) =
  val Γʹ = Γ.bind(name, TypeRepr.from(vType))

  for
    value <- typecheck(value, Γʹ)
    body  <- typecheck(body, Γʹ)
  yield Typing(LetRec(Variable(name, value.repr), value.expr, body.expr), body.repr)

private def checkFun(param: String, x: Type, body: Expr, Γ: TypeEnv) =
  val paramRepr = TypeRepr.from(x)

  typecheck(body, Γ.bind(param, paramRepr)).map: body =>
    Typing(
      Fun(Variable(param, paramRepr), body.expr),
      TypeRepr.Fun(paramRepr, body.repr)
    )

private def checkApply(fun: Expr, arg: Expr, Γ: TypeEnv) =
  typecheck(fun, Γ).flatMap:
    case Typing(fun, TypeRepr.Fun(from, to)) =>
      expect(arg, from, Γ).map: arg =>
        Typing(Apply(fun, arg.expr), to)
    case Typing(_, other) => Left(s"Expected a function but got $other")

def typecheck(expr: Expr, Γ: TypeEnv): Either[String, Typing[?]] =
  expr match
    case Expr.Bool(value)                      => Right(Typing(Bool(value), TypeRepr.Bool))
    case Expr.Num(value)                       => Right(Typing(Num(value), TypeRepr.Num))
    case Expr.Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
    case Expr.Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
    case Expr.Ref(name)                        => checkRef(name, Γ)
    case Expr.Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
    case Expr.Let(name, value, body)           => checkLet(name, value, body, Γ)
    case Expr.LetRec(name, value, vType, body) => checkLetRec(name, value, vType, body, Γ)
    case Expr.Fun(param, pType, body)          => checkFun(param, pType, body, Γ)
    case Expr.Apply(fun, arg)                  => checkApply(fun, arg, Γ)
