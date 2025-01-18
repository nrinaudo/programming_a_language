package typed_simple

import untyped.{Expr, Type}
import TypedExpr.*

// - Type checking results ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Typing[A <: Type](expr: TypedExpr[A], repr: TypeRepr[A]):
  def cast[B <: Type](to: TypeRepr[B]): Either[String, Typing[B]] = this match
    case Typing(expr, `to`) => Right(Typing(expr, to))
    case Typing(_, other)   => Left(s"Expected $to but found $other")

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class TypeEnv private (env: List[TypeEnv.Binding]):
  def bind(name: String, repr: TypeRepr[?]) =
    TypeEnv(TypeEnv.Binding(name, repr) :: env)

  def lookup(name: String) =
    env
      .find(_.name == name)
      .map(_.repr)
      .toRight(s"Type binding $name not found")

object TypeEnv:
  private case class Binding(name: String, repr: TypeRepr[?])

  val empty = TypeEnv(List.empty)

// - Actual type checking ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Attempts to type check `expr` as a `TypedExpr[A]`.
private def expect[A <: Type](expr: Expr, expected: TypeRepr[A], Γ: TypeEnv) =
  for
    raw   <- typeCheck(expr, Γ)
    typed <- raw.cast(expected)
  yield typed

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

private def checkCond(pred: Expr, onT: Expr, onF: Expr, Γ: TypeEnv) =
  for
    pred <- expect(pred, TypeRepr.Bool, Γ)
    onT  <- typeCheck(onT, Γ)
    onF  <- expect(onF, onT.repr, Γ)
  yield Typing(Cond(pred.expr, onT.expr, onF.expr), onT.repr)

private def checkRef(name: String, Γ: TypeEnv) =
  Γ.lookup(name).map(repr => Typing(Ref(name, repr), repr))

private def checkLet(name: String, value: Expr, body: Expr, Γ: TypeEnv) =
  for
    value <- typeCheck(value, Γ)
    body  <- typeCheck(body, Γ.bind(name, value.repr))
  yield Typing(Let(name, value.expr, value.repr, body.expr), body.repr)

private def checkLetRec(name: String, value: Expr, x: Type, body: Expr, Γ: TypeEnv) =
  val xRepr = TypeRepr.from(x)
  val Γʹ    = Γ.bind(name, xRepr)

  for
    value <- expect(value, xRepr, Γʹ)
    body  <- typeCheck(body, Γʹ)
  yield Typing(LetRec(name, value.expr, value.repr, body.expr), body.repr)

private def checkFun(param: String, x: Type, body: Expr, Γ: TypeEnv) =
  val xRepr = TypeRepr.from(x)

  for body <- typeCheck(body, Γ.bind(param, xRepr))
  yield Typing(Fun(param, xRepr, body.expr), xRepr -> body.repr)

private def checkApply(fun: Expr, arg: Expr, Γ: TypeEnv) =
  typeCheck(fun, Γ).flatMap:
    case Typing(fun, x -> y) =>
      expect(arg, x, Γ).map: arg =>
        Typing(Apply(fun, arg.expr), y)
    case Typing(_, other) => Left(s"Expected a function, found $other")

def typeCheck(expr: Expr, Γ: TypeEnv): Either[String, Typing[?]] =
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
