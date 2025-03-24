package typed_ast

import TypedExpr.*

// - Type equality -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

  // Type equality preserves the injectivity of type constructors.
  def congruence[F[_]]: Eq[F[A], F[B]] = this match
    case Refl() => Refl()

  // The compiler knows that `A` and `B` are the same, so will be happy to treat `a` as a `B`.
  def cast(a: A): B = this match
    case Refl() => a

object Eq:
  // Attempts to produce a type quality between A and B.
  def from[A <: Type, B <: Type](a: TypeRepr[A], b: TypeRepr[B]): Either[String, Eq[A, B]] =
    (a, b) match
      case (TypeRepr.Num, TypeRepr.Num)   => Right(Eq.Refl())
      case (TypeRepr.Bool, TypeRepr.Bool) => Right(Eq.Refl())
      case (aFrom -> aTo, bFrom -> bTo) =>
        for
          eqFrom <- from(aFrom, bFrom)
          eqTo   <- from(aTo, bTo)
        yield ((eqFrom, eqTo) match
          case (Refl(), Refl()) => Refl()
        ): Eq[A, B]
      case _ => Left(s"Failed to prove $a = $b")

// - Types -------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Type:
  case Num
  case Bool
  case Fun[X <: Type, Y <: Type](from: X, to: Y)

  def ->(other: Type): Type = Fun(this, other)

type ->[X <: Type, Y <: Type] = Type.Fun[X, Y]

object Type:
  type Num  = Type.Num.type
  type Bool = Type.Bool.type

// - Tagged types ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum TypeRepr[A <: Type]:
  case Num  extends TypeRepr[Type.Num]
  case Bool extends TypeRepr[Type.Bool]
  case Fun[A <: Type, B <: Type](
      from: TypeRepr[A],
      to: TypeRepr[B]
  ) extends TypeRepr[A -> B]

  def ->[B <: Type](other: TypeRepr[B]): TypeRepr.Fun[A, B] = Fun(this, other)

object TypeRepr:
  // This gives us a `TypeRepr` for some `Type`. We don't know what the type parameter is, just that it exists, which
  // is enough for our purposes.
  def from(tpe: Type): TypeRepr[?] = tpe match
    case Type.Bool      => TypeRepr.Bool
    case Type.Num       => TypeRepr.Num
    case Type.Fun(a, b) => TypeRepr.from(a) -> TypeRepr.from(b)

object `->`:
  def unapply[A <: Type, B <: Type](repr: TypeRepr.Fun[A, B]): Some[(TypeRepr[A], TypeRepr[B])] =
    Some(repr.from, repr.to)

// - AST ---------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Expr:
  case Bool(value: Boolean)
  case Num(value: Int)
  case Gt(lhs: Expr, rhs: Expr)
  case Add(lhs: Expr, rhs: Expr)
  case Cond(pred: Expr, onT: Expr, onF: Expr)
  case Let(name: String, value: Expr, vType: Type, body: Expr)
  case LetRec(name: String, value: Expr, vType: Type, body: Expr)
  case Ref(name: String)
  case Fun(param: String, pType: Type, body: Expr)
  case Apply(fun: Expr, arg: Expr)

// - Typed AST ---------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum TypedExpr[A <: Type]:
  case Bool(value: Boolean)                                                              extends TypedExpr[Type.Bool]
  case Num(value: Int)                                                                   extends TypedExpr[Type.Num]
  case Gt(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num])                            extends TypedExpr[Type.Bool]
  case Add(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num])                           extends TypedExpr[Type.Num]
  case Cond[A <: Type](pred: TypedExpr[Type.Bool], onT: TypedExpr[A], onF: TypedExpr[A]) extends TypedExpr[A]
  case Let[A <: Type, B <: Type](name: String, value: TypedExpr[A], vType: TypeRepr[A], body: TypedExpr[B])
      extends TypedExpr[B]
  case LetRec[A <: Type, B <: Type](name: String, value: TypedExpr[A], vType: TypeRepr[A], body: TypedExpr[B])
      extends TypedExpr[B]
  case Ref[A <: Type](name: String, rType: TypeRepr[A])                                 extends TypedExpr[A]
  case Fun[A <: Type, B <: Type](param: String, pType: TypeRepr[A], body: TypedExpr[B]) extends TypedExpr[A -> B]
  case Apply[A <: Type, B <: Type](fun: TypedExpr[A -> B], arg: TypedExpr[A])           extends TypedExpr[B]

// - Interpretation result ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
type Value[X] = X match
  case Type.Num  => Int
  case Type.Bool => Boolean
  case a -> b    => Value[a] => Either[String, Value[b]]

// - Environment -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class Env private (env: List[Env.Binding]):
  def lookup[A <: Type](name: String, repr: TypeRepr[A]) =
    for
      Env.Binding(_, v) <- find(name)
      rawValue          <- Option.fromNullable(v).toRight(s"Expected a $repr but found null")
      value             <- rawValue.as(repr)
    yield value

  def bind[A <: Type](name: String, value: TypedValue[A] | Null): Env =
    Env(Env.Binding(name, value) :: env)

  private def find(name: String) =
    env
      .find(_.name == name)
      .toRight(s"Binding not found: $name")

  def set[A <: Type](name: String, value: TypedValue[A]) =
    find(name)
      .map(_.value = value)

object Env:
  private case class Binding(name: String, var value: TypedValue[?] | Null)

  val empty: Env = Env(List.empty)

case class TypedValue[A <: Type](value: Value[A], repr: TypeRepr[A]):
  def as[B <: Type](to: TypeRepr[B]): Either[String, Value[B]] =
    Eq.from(repr, to)
      .map(_.congruence[Value].cast(value))

// - Interpretation ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private def runAdd(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num], e: Env) =
  for
    lhs <- interpret(lhs, e)
    rhs <- interpret(rhs, e)
  yield lhs + rhs

private def runGt(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num], e: Env) =
  for
    lhs <- interpret(lhs, e)
    rhs <- interpret(rhs, e)
  yield lhs > rhs

private def runCond[X <: Type](pred: TypedExpr[Type.Bool], onT: TypedExpr[X], onF: TypedExpr[X], e: Env) =
  interpret(pred, e).flatMap:
    case true  => interpret(onT, e)
    case false => interpret(onF, e)

private def runLet[X <: Type, Y <: Type](
    name: String,
    value: TypedExpr[X],
    vType: TypeRepr[X],
    body: TypedExpr[Y],
    e: Env
) =
  for
    value <- interpret(value, e)
    body  <- interpret(body, e.bind(name, TypedValue(value, vType)))
  yield body

private def runRef[X <: Type](name: String, rType: TypeRepr[X], e: Env) =
  e.lookup(name, rType)

private def runLetRec[X <: Type, Y <: Type](
    name: String,
    value: TypedExpr[X],
    vType: TypeRepr[X],
    body: TypedExpr[Y],
    e: Env
) =
  val eʹ = e.bind(name, null)

  for
    value <- interpret(value, eʹ)
    _     <- eʹ.set(name, TypedValue(value, vType))
    body  <- interpret(body, eʹ)
  yield body

private def runFun[X <: Type, Y <: Type](param: String, pType: TypeRepr[X], body: TypedExpr[Y], e: Env) =
  Right: (x: Value[X]) =>
    interpret(body, e.bind(param, TypedValue(x, pType)))

private def runApply[X <: Type, Y <: Type](fun: TypedExpr[X -> Y], arg: TypedExpr[X], e: Env) =
  for
    fun <- interpret(fun, e)
    arg <- interpret(arg, e)
    y   <- fun(arg)
  yield y

def interpret[A <: Type](expr: TypedExpr[A], e: Env): Either[String, Value[A]] =
  expr match
    case Bool(value)                      => Right(value)
    case Num(value)                       => Right(value)
    case Add(lhs, rhs)                    => runAdd(lhs, rhs, e)
    case Gt(lhs, rhs)                     => runGt(lhs, rhs, e)
    case Cond(pred, onT, onF)             => runCond(pred, onT, onF, e)
    case Let(name, value, vType, body)    => runLet(name, value, vType, body, e)
    case LetRec(name, value, vType, body) => runLetRec(name, value, vType, body, e)
    case Ref(name, rType)                 => runRef(name, rType, e)
    case Apply(fun, arg)                  => runApply(fun, arg, e)
    // I really wish the runtime cast wasn't necessary, but: https://github.com/scala/scala3/issues/21391
    case Fun(param, pType, body) => runFun(param, pType, body, e).map(_.asInstanceOf[Value[A]])

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

// - Type checking -----------------------------------------------------------------------------------------------------
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

private def checkLet(name: String, value: Expr, vType: TypeRepr[?], body: Expr, Γ: TypeEnv) =
  for
    value <- expect(value, vType, Γ)
    body  <- typeCheck(body, Γ.bind(name, value.repr))
  yield Typing(Let(name, value.expr, value.repr, body.expr), body.repr)

private def checkLetRec(name: String, value: Expr, x: TypeRepr[?], body: Expr, Γ: TypeEnv) =
  val Γʹ = Γ.bind(name, x)

  for
    value <- expect(value, x, Γʹ)
    body  <- typeCheck(body, Γʹ)
  yield Typing(LetRec(name, value.expr, value.repr, body.expr), body.repr)

private def checkFun(param: String, x: TypeRepr[?], body: Expr, Γ: TypeEnv) =
  for body <- typeCheck(body, Γ.bind(param, x))
  yield Typing(Fun(param, x, body.expr), x -> body.repr)

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
    case Expr.Let(name, value, vType, body)    => checkLet(name, value, TypeRepr.from(vType), body, Γ)
    case Expr.LetRec(name, value, vType, body) => checkLetRec(name, value, TypeRepr.from(vType), body, Γ)
    case Expr.Fun(param, pType, body)          => checkFun(param, TypeRepr.from(pType), body, Γ)
    case Expr.Apply(fun, arg)                  => checkApply(fun, arg, Γ)
