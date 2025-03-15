package type_inference

import Expr.*

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

// - Type inference ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

def infer(
    expr: Expr[Option[Type]],
    Γ: TypeEnv
): Either[String, Expr[Type]] =
  val state = InfState()

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

  def checkFun(param: String, x: TypeInf, body: Expr[Option[Type]], Γ: TypeEnv) =
    for Typing(body, y) <- typeCheck(body, Γ.bind(param, x))
    yield Typing(Fun(param, x, body), x -> y)

  def checkApply(fun: Expr[Option[Type]], arg: Expr[Option[Type]], Γ: TypeEnv) =
    val y = state.freshVar
    for
      Typing(arg, x) <- typeCheck(arg, Γ)
      fun            <- expect(fun, x -> y, Γ)
    yield Typing(Apply(fun, arg), y)

  def checkLetRec(name: String, value: Expr[Option[Type]], x: TypeInf, body: Expr[Option[Type]], Γ: TypeEnv) =
    val Γʹ = Γ.bind(name, x)
    for
      value           <- expect(value, x, Γʹ)
      Typing(body, y) <- typeCheck(body, Γʹ)
    yield Typing(LetRec(name, value, x, body), y)

  def checkRef(name: String, Γ: TypeEnv) =
    for t <- Γ.lookup(name)
    yield Typing(Ref(name), t)

  def expect(expr: Expr[Option[Type]], t: TypeInf, Γ: TypeEnv) =
    for
      Typing(expr, t2) <- typeCheck(expr, Γ)
      _                <- state.unify(t, t2)
    yield expr

  def typeCheck(expr: Expr[Option[Type]], Γ: TypeEnv): Either[String, Typing] =
    expr match
      case Bool(value)                      => Right(Typing(Bool(value), TypeInf.Bool))
      case Num(value)                       => Right(Typing(Num(value), TypeInf.Num))
      case Gt(lhs, rhs)                     => checkGt(lhs, rhs, Γ)
      case Add(lhs, rhs)                    => checkAdd(lhs, rhs, Γ)
      case Cond(pred, onT, onF)             => checkCond(pred, onT, onF, Γ)
      case Let(name, value, vType, body)    => checkLet(name, value, state.toInf(vType), body, Γ)
      case LetRec(name, value, vType, body) => checkLetRec(name, value, state.toInf(vType), body, Γ)
      case Ref(name)                        => checkRef(name, Γ)
      case Fun(param, pType, body)          => checkFun(param, state.toInf(pType), body, Γ)
      case Apply(fun, arg)                  => checkApply(fun, arg, Γ)

  // - Substitution ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def substGt(lhs: Expr[TypeInf], rhs: Expr[TypeInf]) =
    for
      lhs <- substitute(lhs)
      rhs <- substitute(rhs)
    yield Gt(lhs, rhs)

  def substAdd(lhs: Expr[TypeInf], rhs: Expr[TypeInf]) =
    for
      lhs <- substitute(lhs)
      rhs <- substitute(rhs)
    yield Add(lhs, rhs)

  def substCond(pred: Expr[TypeInf], onT: Expr[TypeInf], onF: Expr[TypeInf]) =
    for
      pred <- substitute(pred)
      onT  <- substitute(onT)
      onF  <- substitute(onF)
    yield Cond(pred, onT, onF)

  def substLet(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf]) =
    for
      value <- substitute(value)
      body  <- substitute(body)
      x     <- state.getType(x)
    yield Let(name, value, x, body)

  def substFun(param: String, x: TypeInf, body: Expr[TypeInf]) =
    for
      body <- substitute(body)
      x    <- state.getType(x)
    yield Fun(param, x, body)

  def substApply(fun: Expr[TypeInf], arg: Expr[TypeInf]) =
    val y = state.freshVar
    for
      arg <- substitute(arg)
      fun <- substitute(fun)
    yield Apply(fun, arg)

  def substLetRec(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf]) =
    for
      value <- substitute(value)
      body  <- substitute(body)
      x     <- state.getType(x)
    yield LetRec(name, value, x, body)

  def substRef(name: String): Either[String, Expr[Type]] = Right(Ref(name))

  def substitute(expr: Expr[TypeInf]): Either[String, Expr[Type]] =
    expr match
      case Bool(value)                      => Right(Bool(value))
      case Num(value)                       => Right(Num(value))
      case Gt(lhs, rhs)                     => substGt(lhs, rhs)
      case Add(lhs, rhs)                    => substAdd(lhs, rhs)
      case Cond(pred, onT, onF)             => substCond(pred, onT, onF)
      case Let(name, value, vType, body)    => substLet(name, value, vType, body)
      case LetRec(name, value, vType, body) => substLetRec(name, value, vType, body)
      case Ref(name)                        => substRef(name)
      case Fun(param, pType, body)          => substFun(param, pType, body)
      case Apply(fun, arg)                  => substApply(fun, arg)

  for
    Typing(expr, _) <- typeCheck(expr, Γ)
    expr            <- substitute(expr)
  yield expr
