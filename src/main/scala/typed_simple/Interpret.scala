package typed_simple

import untyped.{->, Type}
import typed_simple.*
import typed_simple.TypedExpr.*

// - Interpretation result ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
type Value[X] = X match
  case Type.Num  => Int
  case Type.Bool => Boolean
  case a -> b    => Value[a] => Either[String, Value[b]]

// - Environment -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private class Env private (env: List[Env.Binding]):
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

def runAdd(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num], e: Env) =
  for
    lhs <- interpret(lhs, e)
    rhs <- interpret(rhs, e)
  yield lhs + rhs

def runGt(lhs: TypedExpr[Type.Num], rhs: TypedExpr[Type.Num], e: Env) =
  for
    lhs <- interpret(lhs, e)
    rhs <- interpret(rhs, e)
  yield lhs > rhs

def runCond[X <: Type](pred: TypedExpr[Type.Bool], onT: TypedExpr[X], onF: TypedExpr[X], e: Env) =
  interpret(pred, e).flatMap:
    case true  => interpret(onT, e)
    case false => interpret(onF, e)

def runLet[X <: Type, Y <: Type](name: String, value: TypedExpr[X], vType: TypeRepr[X], body: TypedExpr[Y], e: Env) =
  for
    value <- interpret(value, e)
    body  <- interpret(body, e.bind(name, TypedValue(value, vType)))
  yield body

def runRef[X <: Type](name: String, rType: TypeRepr[X], e: Env) =
  e.lookup(name, rType)

def runLetRec[X <: Type, Y <: Type](name: String, value: TypedExpr[X], vType: TypeRepr[X], body: TypedExpr[Y], e: Env) =
  val eʹ = e.bind(name, null)

  for
    value <- interpret(value, eʹ)
    _     <- eʹ.set(name, TypedValue(value, vType))
    body  <- interpret(body, eʹ)
  yield body

def runFun[X <: Type, Y <: Type](param: String, pType: TypeRepr[X], body: TypedExpr[Y], e: Env) =
  Right: (x: Value[X]) =>
    interpret(body, e.bind(param, TypedValue(x, pType)))

def runApply[X <: Type, Y <: Type](fun: TypedExpr[X -> Y], arg: TypedExpr[X], e: Env) =
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
