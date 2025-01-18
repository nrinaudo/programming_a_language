package typed_env

import untyped.{->, Expr, Type}
import TypedExpr.*

// - Type checking result ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Typing[X <: Type, Γ <: Tuple](
    expr: TypedExpr[X, Γ],
    repr: TypeRepr[X]
):
  def cast[Y <: Type](to: TypeRepr[Y]): Either[String, Typing[Y, Γ]] = this match
    case Typing(expr, `to`) => Right(Typing(expr, to))
    case Typing(_, other)   => Left(s"Expected type $repr but found $other")

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum TypeEnv[Γ <: Tuple]:
  case Empty                                                                    extends TypeEnv[EmptyTuple]
  case NonEmpty[Γ <: NonEmptyTuple](values: Map[String, TypeEnv.Binding[?, Γ]]) extends TypeEnv[Γ]

  def bind[X <: Type](name: String, repr: TypeRepr[X]): TypeEnv[X *: Γ] =
    val newBinding: TypeEnv.Binding[X, X *: Γ] = TypeEnv.Binding(Elem.Here(), repr)

    this match
      case TypeEnv.Empty => TypeEnv.NonEmpty(Map(name -> newBinding))

      case TypeEnv.NonEmpty(data) =>
        def nest[Y <: Type](binding: TypeEnv.Binding[Y, Γ]): TypeEnv.Binding[Y, X *: Γ] =
          binding.copy(path = Elem.There(binding.path))

        val newData = data.view.mapValues(nest).toMap

        TypeEnv.NonEmpty[X *: Γ](newData + (name -> newBinding))

object TypeEnv:
  case class Binding[X <: Type, Γ <: NonEmptyTuple](
      path: Elem[X, Γ],
      repr: TypeRepr[X]
  )

// - Type checking -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Helper to check that an untyped expression type-checks to the expected type.
private def expect[X <: Type, Γ <: Tuple](expr: Expr, expected: TypeRepr[X], γ: TypeEnv[Γ]) =
  for
    raw   <- typeCheck(expr, γ)
    typed <- raw.cast(expected)
  yield typed

private def checkAdd[Γ <: Tuple](lhs: Expr, rhs: Expr, γ: TypeEnv[Γ]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, γ)
    rhs <- expect(rhs, TypeRepr.Num, γ)
  yield Typing(Add(lhs.expr, rhs.expr), TypeRepr.Num)

private def checkGt[Γ <: Tuple](lhs: Expr, rhs: Expr, γ: TypeEnv[Γ]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, γ)
    rhs <- expect(rhs, TypeRepr.Num, γ)
  yield Typing(Gt(lhs.expr, rhs.expr), TypeRepr.Bool)

private def checkCond[X <: Tuple, Γ <: Tuple](
    pred: Expr,
    onT: Expr,
    onF: Expr,
    γ: TypeEnv[Γ]
) =
  for
    pred <- expect(pred, TypeRepr.Bool, γ)
    onT  <- typeCheck(onT, γ)
    onF  <- expect(onF, onT.repr, γ)
  yield Typing(Cond(pred.expr, onT.expr, onF.expr), onT.repr)

private def checkRef[Γ <: Tuple](name: String, γ: TypeEnv[Γ]) =
  γ match
    case TypeEnv.Empty => Left(s"Binding not found: $name")
    case TypeEnv.NonEmpty(data) =>
      data
        .get(name)
        .toRight(s"Binding not found: $name")
        .map:
          case TypeEnv.Binding(path, repr) => Typing(Ref(path), repr)

private def checkLet[Γ <: Tuple](
    name: String,
    value: Expr,
    body: Expr,
    γ: TypeEnv[Γ]
) =
  for
    value <- typeCheck(value, γ)
    body  <- typeCheck(body, γ.bind(name, value.repr))
  yield Typing(Let(value.expr, body.expr), body.repr)

private def checkLetRec[Γ <: Tuple](
    name: String,
    value: Expr,
    x: Type,
    body: Expr,
    γ: TypeEnv[Γ]
) =
  val xRepr = TypeRepr.from(x)
  val γʹ    = γ.bind(name, xRepr)

  for
    value <- expect(value, xRepr, γʹ)
    body  <- typeCheck(body, γʹ)
  yield Typing(LetRec(value.expr, body.expr), body.repr)

private def checkFun[Γ <: Tuple](param: String, x: Type, body: Expr, γ: TypeEnv[Γ]) =
  val xRepr = TypeRepr.from(x)

  for body <- typeCheck(body, γ.bind(param, xRepr))
  yield Typing(Fun(body.expr), xRepr -> body.repr)

private def checkApply[Γ <: Tuple](fun: Expr, arg: Expr, γ: TypeEnv[Γ]) =
  typeCheck(fun, γ).flatMap:
    case Typing(fun, x -> y) =>
      expect(arg, x, γ).map: arg =>
        Typing(Apply(fun, arg.expr), y)
    case Typing(_, other) => Left(s"Expected a function, found $other")

def typeCheck[Γ <: Tuple](
    expr: Expr,
    γ: TypeEnv[Γ]
): Either[String, Typing[?, Γ]] =
  expr match
    case Expr.Bool(value)                      => Right(Typing(Bool(value), TypeRepr.Bool))
    case Expr.Num(value)                       => Right(Typing(Num(value), TypeRepr.Num))
    case Expr.Add(lhs, rhs)                    => checkAdd(lhs, rhs, γ)
    case Expr.Gt(lhs, rhs)                     => checkGt(lhs, rhs, γ)
    case Expr.Cond(pred, onT, onF)             => checkCond(pred, onT, onF, γ)
    case Expr.Let(name, value, body)           => checkLet(name, value, body, γ)
    case Expr.LetRec(name, value, vType, body) => checkLetRec(name, value, vType, body, γ)
    case Expr.Ref(name)                        => checkRef(name, γ)
    case Expr.Fun(param, pType, body)          => checkFun(param, pType, body, γ)
    case Expr.Apply(fun, arg)                  => checkApply(fun, arg, γ)
