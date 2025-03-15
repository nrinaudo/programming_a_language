package typed_env

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
private def expect[X <: Type, Γ <: Tuple](expr: Expr, expected: TypeRepr[X], Γ: TypeEnv[Γ]) =
  for
    raw   <- typeCheck(expr, Γ)
    typed <- raw.cast(expected)
  yield typed

private def checkAdd[Γ <: Tuple](lhs: Expr, rhs: Expr, Γ: TypeEnv[Γ]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, Γ)
    rhs <- expect(rhs, TypeRepr.Num, Γ)
  yield Typing(Add(lhs.expr, rhs.expr), TypeRepr.Num)

private def checkGt[Γ <: Tuple](lhs: Expr, rhs: Expr, Γ: TypeEnv[Γ]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, Γ)
    rhs <- expect(rhs, TypeRepr.Num, Γ)
  yield Typing(Gt(lhs.expr, rhs.expr), TypeRepr.Bool)

private def checkCond[X <: Tuple, Γ <: Tuple](pred: Expr, onT: Expr, onF: Expr, Γ: TypeEnv[Γ]) =
  for
    pred <- expect(pred, TypeRepr.Bool, Γ)
    onT  <- typeCheck(onT, Γ)
    onF  <- expect(onF, onT.repr, Γ)
  yield Typing(Cond(pred.expr, onT.expr, onF.expr), onT.repr)

private def checkRef[Γ <: Tuple](name: String, Γ: TypeEnv[Γ]) =
  Γ match
    case TypeEnv.Empty => Left(s"Binding not found: $name")
    case TypeEnv.NonEmpty(data) =>
      data
        .get(name)
        .toRight(s"Binding not found: $name")
        .map:
          case TypeEnv.Binding(path, repr) => Typing(Ref(path), repr)

private def checkLet[Γ <: Tuple](name: String, value: Expr, x: TypeRepr[?], body: Expr, Γ: TypeEnv[Γ]) =
  for
    value <- expect(value, x, Γ)
    body  <- typeCheck(body, Γ.bind(name, value.repr))
  yield Typing(Let(value.expr, body.expr), body.repr)

private def checkLetRec[Γ <: Tuple](name: String, value: Expr, x: TypeRepr[?], body: Expr, Γ: TypeEnv[Γ]) =
  val Γʹ = Γ.bind(name, x)

  for
    value <- expect(value, x, Γʹ)
    body  <- typeCheck(body, Γʹ)
  yield Typing(LetRec(value.expr, body.expr), body.repr)

private def checkFun[Γ <: Tuple](param: String, x: TypeRepr[?], body: Expr, Γ: TypeEnv[Γ]) =
  for body <- typeCheck(body, Γ.bind(param, x))
  yield Typing(Fun(body.expr), x -> body.repr)

private def checkApply[Γ <: Tuple](fun: Expr, arg: Expr, Γ: TypeEnv[Γ]) =
  typeCheck(fun, Γ).flatMap:
    case Typing(fun, x -> y) =>
      expect(arg, x, Γ).map: arg =>
        Typing(Apply(fun, arg.expr), y)
    case Typing(_, other) => Left(s"Expected a function, found $other")

def typeCheck[Γ <: Tuple](
    expr: Expr,
    Γ: TypeEnv[Γ]
): Either[String, Typing[?, Γ]] =
  expr match
    case Expr.Bool(value)                      => Right(Typing(Bool(value), TypeRepr.Bool))
    case Expr.Num(value)                       => Right(Typing(Num(value), TypeRepr.Num))
    case Expr.Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
    case Expr.Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
    case Expr.Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
    case Expr.Let(name, value, vType, body)    => checkLet(name, value, TypeRepr.from(vType), body, Γ)
    case Expr.LetRec(name, value, vType, body) => checkLetRec(name, value, TypeRepr.from(vType), body, Γ)
    case Expr.Ref(name)                        => checkRef(name, Γ)
    case Expr.Fun(param, pType, body)          => checkFun(param, TypeRepr.from(pType), body, Γ)
    case Expr.Apply(fun, arg)                  => checkApply(fun, arg, Γ)
