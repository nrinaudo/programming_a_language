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
  def bind[A <: Type](variable: Variable[A], value: Value[A]) = Env(
    Env.Binding(variable.name, Env.TypedValue(value, variable.tpe)) :: env
  )

  // That unfortunately cannot be replaced with `bind(name, null)`, because match types are fiddly.
  def init[A <: Type](variable: Variable[A]) =
    Env(Env.Binding(variable.name, null) :: env)

  private def find(name: String): Either[String, Env.Binding] =
    env.find(_.name == name).toRight(s"Binding not found: $name")

  def set[A <: Type](variable: Variable[A], value: Value[A]) =
    find(variable.name)
      .map: binding =>
        binding.value = Env.TypedValue(value, variable.tpe)
        this

  def lookup[A <: Type](variable: Variable[A]) =
    for
      binding <- find(variable.name)
      value   <- binding.value.cast(variable.tpe)
    yield value

object Env:
  private case class Binding(name: String, var value: TypedValue[?])
  private case class TypedValue[A <: Type](value: Value[A], repr: TypeRepr[A]):
    def cast[B <: Type](to: TypeRepr[B]): Either[String, Value[B]] = repr.cast(value, to)

  val empty: Env = Env(List.empty)

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

def runLet[X <: Type, Y <: Type](variable: Variable[X], value: TypedExpr[X], body: TypedExpr[Y], e: Env) =
  for
    value <- interpret(value, e)
    body  <- interpret(body, e.bind(variable, value))
  yield body

def runLetRec[X <: Type, Y <: Type](variable: Variable[X], value: TypedExpr[X], body: TypedExpr[Y], e: Env) =
  val eʹ = e.init(variable)

  for
    value <- interpret(value, eʹ)
    _     <- eʹ.set(variable, value)
    body  <- interpret(body, eʹ)
  yield body

def runFun[X <: Type, Y <: Type](param: Variable[X], body: TypedExpr[Y], e: Env) =
  Right((x: Value[X]) => interpret(body, e.bind(param, x)))

def runApply[X <: Type, Y <: Type](fun: TypedExpr[X -> Y], arg: TypedExpr[X], e: Env) =
  for
    fun <- interpret(fun, e)
    arg <- interpret(arg, e)
    y   <- fun(arg)
  yield y

def interpret[A <: Type](expr: TypedExpr[A], e: Env): Either[String, Value[A]] =
  expr match
    case Bool(value)                   => Right(value)
    case Num(value)                    => Right(value)
    case Add(lhs, rhs)                 => runAdd(lhs, rhs, e)
    case Gt(lhs, rhs)                  => runGt(lhs, rhs, e)
    case Cond(pred, onT, onF)          => runCond(pred, onT, onF, e)
    case Let(variable, value, body)    => runLet(variable, value, body, e)
    case LetRec(variable, value, body) => runLetRec(variable, value, body, e)
    case Ref(variable)                 => e.lookup(variable)
    case Apply(fun, arg)               => runApply(fun, arg, e)
    // I really wish the runtime cast wasn't necessary, but: https://github.com/scala/scala3/issues/21391
    case Fun(param, body) => runFun(param, body, e).map(_.asInstanceOf[Value[A]])
