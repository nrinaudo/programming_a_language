package typed_env

import untyped.{->, Expr, Type}

// - Type environment --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Associates a location in the environment with its type.
case class TypedLoc[X <: Type, XS <: NonEmptyTuple](
    loc: Elem[X, XS],
    repr: TypeRepr[X]
)

enum TypeEnv[XS <: Tuple]:
  case Empty                                                               extends TypeEnv[EmptyTuple]
  case NonEmpty[XS <: NonEmptyTuple](values: Map[String, TypedLoc[?, XS]]) extends TypeEnv[XS]

  def bind[X <: Type](name: String, repr: TypeRepr[X]): TypeEnv[X *: XS] =
    val newLoc: TypedLoc[X, X *: XS] = TypedLoc(Elem.Here(), repr)

    this match
      case TypeEnv.Empty => TypeEnv.NonEmpty(Map(name -> newLoc))
      case TypeEnv.NonEmpty(data) =>
        def nest[Y <: Type](loc: TypedLoc[Y, XS]): TypedLoc[Y, X *: XS] = loc.copy(loc = Elem.There(loc.loc))

        val newData: Map[String, TypedLoc[?, X *: XS]] = data.view.mapValues(nest).toMap

        TypeEnv.NonEmpty[X *: XS](newData + (name -> newLoc))

// - Type checking -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Result of type checking.
case class Typing[X <: Type, XS <: Tuple](
    expr: TypedExpr[X, XS],
    repr: TypeRepr[X]
):
  def cast[Y <: Type](repr: TypeRepr[Y]): Either[String, TypedExpr[Y, XS]] = this match
    case Typing(expr, `repr`) => Right(expr)
    case _                    => Left(s"Expected type $repr but found ${this.repr}")

// Helper to check that an untyped AST type-checks to the expected type.
def expect[X <: Type, XS <: Tuple](
    expr: Expr,
    expected: TypeRepr[X],
    env: TypeEnv[XS]
) =
  typecheck(expr, env).flatMap:
    case Typing(expr, `expected`) => Right(Typing(expr, expected))
    case Typing(_, other)         => Left(s"Expected type $expected, got $other")

def checkAdd[XS <: Tuple](lhs: Expr, rhs: Expr, env: TypeEnv[XS]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, env)
    rhs <- expect(rhs, TypeRepr.Num, env)
  yield Typing(TypedExpr.Add(lhs.expr, rhs.expr), TypeRepr.Num)

def checkGt[XS <: Tuple](lhs: Expr, rhs: Expr, env: TypeEnv[XS]) =
  for
    lhs <- expect(lhs, TypeRepr.Num, env)
    rhs <- expect(rhs, TypeRepr.Num, env)
  yield Typing(TypedExpr.Gt(lhs.expr, rhs.expr), TypeRepr.Bool)

def checkCond[X <: Tuple, XS <: Tuple](
    pred: Expr,
    onT: Expr,
    onF: Expr,
    env: TypeEnv[XS]
) =
  for
    pred <- expect(pred, TypeRepr.Bool, env)
    onT  <- typecheck(onT, env)
    onF  <- expect(onF, onT.repr, env)
  yield Typing(TypedExpr.Cond(pred.expr, onT.expr, onF.expr), onT.repr)

def checkRef[XS <: Tuple](name: String, env: TypeEnv[XS]) =
  env match
    case TypeEnv.Empty => Left(s"Binding not found: $name")
    case TypeEnv.NonEmpty(data) =>
      data
        .get(name)
        .toRight(s"Binding not found: $name")
        .map:
          case TypedLoc(loc, repr) => Typing(TypedExpr.Ref(loc), repr)

def checkLetRec[XS <: Tuple](
    name: String,
    value: Expr,
    vType: Type,
    body: Expr,
    env: TypeEnv[XS]
) =
  val vRepr = TypeRepr.from(vType)
  val env2  = env.bind(name, vRepr)

  for
    value <- expect(value, vRepr, env2)
    body  <- typecheck(body, env2)
  yield Typing(
    TypedExpr.LetRec(value.expr, body.expr),
    body.repr
  )

def checkLet[XS <: Tuple](
    name: String,
    value: Expr,
    body: Expr,
    env: TypeEnv[XS]
) =
  for
    value <- typecheck(value, env)
    body  <- typecheck(body, env.bind(name, value.repr))
  yield Typing(TypedExpr.Let(value.expr, body.expr), body.repr)

def checkFun[XS <: Tuple](
    param: String,
    pType: Type,
    body: Expr,
    env: TypeEnv[XS]
) =
  val paramRepr = TypeRepr.from(pType)

  typecheck(body, env.bind(param, paramRepr)).map: body =>
    Typing(
      TypedExpr.Fun(body.expr),
      TypeRepr.Fun(paramRepr, body.repr)
    )

def checkApply[XS <: Tuple](fun: Expr, arg: Expr, env: TypeEnv[XS]) =
  typecheck(fun, env).flatMap:
    case Typing(fun, TypeRepr.Fun(from, to)) =>
      expect(arg, from, env).map: arg =>
        Typing(TypedExpr.Apply(fun, arg.expr), to)
    case Typing(_, other) => Left(s"Expected a function but got $other")

def typecheck[XS <: Tuple](
    expr: Expr,
    env: TypeEnv[XS]
): Either[String, Typing[?, XS]] =
  expr match
    case Expr.Bool(value)                      => Right(Typing(TypedExpr.Bool(value), TypeRepr.Bool))
    case Expr.Num(value)                       => Right(Typing(TypedExpr.Num(value), TypeRepr.Num))
    case Expr.Add(lhs, rhs)                    => checkAdd(lhs, rhs, env)
    case Expr.Gt(lhs, rhs)                     => checkGt(lhs, rhs, env)
    case Expr.Cond(pred, onT, onF)             => checkCond(pred, onT, onF, env)
    case Expr.Let(name, value, body)           => checkLet(name, value, body, env)
    case Expr.LetRec(name, value, vType, body) => checkLetRec(name, value, vType, body, env)
    case Expr.Ref(name)                        => checkRef(name, env)
    case Expr.Fun(param, pType, body)          => checkFun(param, pType, body, env)
    case Expr.Apply(fun, arg)                  => checkApply(fun, arg, env)
