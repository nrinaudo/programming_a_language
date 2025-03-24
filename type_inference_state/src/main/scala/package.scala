package type_inference

import Expr.*
import Inferrer.*

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

// - Inference types ---------------------------------------------------------------------------------------------------
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
case class InfState(curr: Int, ϕ: Map[Int, TypeInf]):
  def set(i: Int, t: TypeInf) = copy(ϕ = ϕ + (i -> t))
  def get(i: Int)             = ϕ.get(i)

  def getType(t: TypeInf): Either[String, Type] = t match
    case TypeInf.Num  => Right(Type.Num)
    case TypeInf.Bool => Right(Type.Bool)
    case TypeInf.Fun(x, y) =>
      for
        x <- getType(x)
        y <- getType(y)
      yield Type.Fun(x, y)
    case TypeInf.Var(i) =>
      ϕ.get(i) match
        case None     => Left(s"Failed to infer type for variable $$$i")
        case Some(t2) => getType(t2)

object InfState:
  def empty: InfState = InfState(0, Map.empty)

// - "State" monad -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
case class Inferrer[A](run: InfState => Either[String, Inferrer.Result[A]]):
  def map[B](f: A => B): Inferrer[B] = Inferrer: state =>
    run(state).map: result =>
      result.map(f)

  def flatMap[B](f: A => Inferrer[B]): Inferrer[B] = Inferrer: state =>
    run(state).flatMap: result =>
      f(result.value).run(result.state)

object Inferrer:
  case class Result[A](state: InfState, value: A):
    def map[B](f: A => B): Result[B] = copy(value = f(value))

  def fail[A](err: String): Inferrer[A] = Inferrer: _ =>
    Left(err)

  def freshVar: Inferrer[TypeInf] = Inferrer: state =>
    Right(
      Result(
        state = state.copy(curr = state.curr + 1),
        value = TypeInf.Var(state.curr)
      )
    )

  def get(i: Int): Inferrer[Option[TypeInf]] = Inferrer: state =>
    Right(
      Result(
        state = state,
        value = state.get(i)
      )
    )

  def set(i: Int, t: TypeInf): Inferrer[Unit] = Inferrer: state =>
    Right(
      Result(
        state = state.set(i, t),
        value = ()
      )
    )

  def pure[A](a: A): Inferrer[A] = Inferrer: state =>
    Right(
      Result(
        state = state,
        value = a
      )
    )

  def from[A](e: Either[String, A]): Inferrer[A] = Inferrer: state =>
    e.map(a => Result(state, a))

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

// - Type checking -----------------------------------------------------------------------------------------------------
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

// - Substitution ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
def substGt(lhs: Expr[TypeInf], rhs: Expr[TypeInf], s: InfState) =
  for
    lhs <- subst(lhs, s)
    rhs <- subst(rhs, s)
  yield Gt(lhs, rhs)

def substAdd(lhs: Expr[TypeInf], rhs: Expr[TypeInf], s: InfState) =
  for
    lhs <- subst(lhs, s)
    rhs <- subst(rhs, s)
  yield Add(lhs, rhs)

def substCond(pred: Expr[TypeInf], onT: Expr[TypeInf], onF: Expr[TypeInf], s: InfState) =
  for
    pred <- subst(pred, s)
    onT  <- subst(onT, s)
    onF  <- subst(onF, s)
  yield Cond(pred, onT, onF)

def substLet(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    value <- subst(value, s)
    x     <- getType(x, s)
    body  <- subst(body, s)
  yield Let(name, value, x, body)

def substFun(param: String, x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    x    <- getType(x, s)
    body <- subst(body, s)
  yield Fun(param, x, body)

def substApply(fun: Expr[TypeInf], arg: Expr[TypeInf], s: InfState) =
  for
    arg <- subst(arg, s)
    fun <- subst(fun, s)
  yield Apply(fun, arg)

def substLetRec(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    value <- subst(value, s)
    x     <- getType(x, s)
    body  <- subst(body, s)
  yield LetRec(name, value, x, body)

def substRef(name: String, s: InfState): Either[String, Expr[Type]] = Right(Ref(name))

def getType(t: TypeInf, s: InfState): Either[String, Type] = t match
  case TypeInf.Num  => Right(Type.Num)
  case TypeInf.Bool => Right(Type.Bool)
  case TypeInf.Fun(x, y) =>
    for
      x <- getType(x, s)
      y <- getType(y, s)
    yield Type.Fun(x, y)
  case TypeInf.Var(i) => s.get(i).toRight(s"Variable not resolved: $i").flatMap(getType(_, s))

def subst(expr: Expr[TypeInf], s: InfState): Either[String, Expr[Type]] =
  expr match
    case Bool(value)                      => Right(Bool(value))
    case Num(value)                       => Right(Num(value))
    case Gt(lhs, rhs)                     => substGt(lhs, rhs, s)
    case Add(lhs, rhs)                    => substAdd(lhs, rhs, s)
    case Cond(pred, onT, onF)             => substCond(pred, onT, onF, s)
    case Let(name, value, vType, body)    => substLet(name, value, vType, body, s)
    case LetRec(name, value, vType, body) => substLetRec(name, value, vType, body, s)
    case Ref(name)                        => substRef(name, s)
    case Fun(param, pType, body)          => substFun(param, pType, body, s)
    case Apply(fun, arg)                  => substApply(fun, arg, s)

// - Type inference ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
def infer(expr: Expr[Option[Type]], Γ: TypeEnv): Either[String, Expr[Type]] =
  for
    Inferrer.Result(state, Typing(expr, _)) <- typeCheck(expr, Γ).run(InfState.empty)
    expr                                    <- subst(expr, state)
  yield expr
