package untyped

// - AST ---------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Expr:
  case Bool(value: Boolean)
  case Num(value: Int)
  case Gt(lhs: Expr, rhs: Expr)
  case Add(lhs: Expr, rhs: Expr)
  case Cond(pred: Expr, onT: Expr, onF: Expr)
  case Let(name: String, value: Expr, body: Expr)
  case LetRec(name: String, value: Expr, body: Expr)
  case Ref(name: String)
  case Fun(param: String, body: Expr)
  case Apply(fun: Expr, arg: Expr)

// - Interpretation result ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Value:
  case Num(value: Int)
  case Bool(value: Boolean)
  case Fun(param: String, body: Expr, env: Env)

// - Environment -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
class Env private (env: List[Env.Binding]):
  def lookup(name: String) =
    env
      .find(_.name == name)
      .flatMap(binding => Option.fromNullable(binding.value))
      .getOrElse(sys.error(s"Missing binding: $name"))

  def bind(name: String, value: Value | Null) =
    Env(Env.Binding(name, value) :: env)

  def set(name: String, value: Value) =
    env
      .find(_.name == name)
      .map(_.value = value)

object Env:
  private case class Binding(name: String, var value: Value | Null)
  val empty: Env = Env(List.empty)

// - Interpretation ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private def typeError(term: String) = sys.error(s"Type error when interpreting $term")

private def runAdd(lhs: Expr, rhs: Expr, e: Env) =
  (interpret(lhs, e), interpret(rhs, e)) match
    case (Value.Num(lhs), Value.Num(rhs)) => Value.Num(lhs + rhs)
    case _                                => typeError("add")

private def runGt(lhs: Expr, rhs: Expr, e: Env) =
  (interpret(lhs, e), interpret(rhs, e)) match
    case (Value.Num(lhs), Value.Num(rhs)) => Value.Bool(lhs > rhs)
    case _                                => typeError("gt")

private def runCond(pred: Expr, onT: Expr, onF: Expr, e: Env) =
  interpret(pred, e) match
    case Value.Bool(true)  => interpret(onT, e)
    case Value.Bool(false) => interpret(onF, e)
    case _                 => typeError("cond")

private def runLet(name: String, value: Expr, body: Expr, e: Env) =
  val v = interpret(value, e)
  interpret(body, e.bind(name, v))

private def runLetRec(name: String, value: Expr, body: Expr, e: Env) =
  val e2 = e.bind(name, null)
  val v  = interpret(value, e2)
  e2.set(name, v)
  interpret(body, e2)

private def runApply(fun: Expr, arg: Expr, e: Env) =
  interpret(fun, e) match
    case Value.Fun(param, body, e2) =>
      val v = interpret(arg, e)
      interpret(body, e2.bind(param, v))
    case _ => typeError("apply")

def interpret(expr: Expr, e: Env): Value = expr match
  case Expr.Bool(value)               => Value.Bool(value)
  case Expr.Num(value)                => Value.Num(value)
  case Expr.Gt(lhs, rhs)              => runGt(lhs, rhs, e)
  case Expr.Add(lhs, rhs)             => runAdd(lhs, rhs, e)
  case Expr.Cond(pred, onT, onF)      => runCond(pred, onT, onF, e)
  case Expr.Let(name, value, body)    => runLet(name, value, body, e)
  case Expr.LetRec(name, value, body) => runLetRec(name, value, body, e)
  case Expr.Ref(name)                 => e.lookup(name)
  case Expr.Fun(param, body)          => Value.Fun(param, body, e)
  case Expr.Apply(fun, arg)           => runApply(fun, arg, e)
