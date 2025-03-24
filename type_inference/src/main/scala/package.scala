package type_inference

import Expr.*

// - AST ---------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Expr[T]:
  case Bool(value: Boolean)
  case Num(value: Int)
  case Gt(lhs: Expr[T], rhs: Expr[T])
  case Add(lhs: Expr[T], rhs: Expr[T])
  case Cond(pred: Expr[T], onT: Expr[T], onF: Expr[T])
  case Let(name: String, value: Expr[T], vType: T, body: Expr[T])
  case LetRec(name: String, value: Expr[T], vType: T, body: Expr[T])
  case Ref(name: String)
  case Fun(param: String, pType: T, body: Expr[T])
  case Apply(fun: Expr[T], arg: Expr[T])

// - Type --------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Type:
  case Num
  case Bool
  case Fun[X <: Type, Y <: Type](from: X, to: Y)
  def ->(t: Type): Type = Fun(this, t)

object Type:
  type Num  = Type.Num.type
  type Bool = Type.Bool.type

// - Inferred types ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum TypeInf:
  case Num
  case Bool
  case Fun(from: TypeInf, to: TypeInf)
  case Var(index: Int)
  def ->(t: TypeInf): TypeInf = Fun(this, t)

object TypeInf:
  def from(tpe: Type): TypeInf = tpe match
    case Type.Num       => TypeInf.Num
    case Type.Bool      => TypeInf.Bool
    case Type.Fun(a, b) => from(a) -> from(b)

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
      yield x -> y
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
