package typed_env

// - Interpretation result ---------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
type Value[X <: Type] = X match
  case Type.Num  => Int
  case Type.Bool => Boolean
  case a -> b    => Value[a] => Value[b]

// - Environment -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Environment that mirrors the structure of a tuple of Type, but stores Values instead.
// This is how we have a fully typed environment: we know exactly the type of each value at a given index,
// and use Elem to access it.
enum Env[Γ <: Tuple]:
  // The empty environment.
  case Empty extends Env[EmptyTuple]

  // The non empty environment.
  // Note that the associated value must both be nullable and mutable for recursion.
  case Cons[X <: Type, Γ <: Tuple](var value: Value[X] | Null, tail: Env[Γ]) extends Env[X *: Γ]

  def push[X <: Type](value: Value[X] | Null): Cons[X, Γ] = Env.Cons(value, this)

// - Interpretation ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private def runAdd[Γ <: Tuple](lhs: TypedExpr[Type.Num, Γ], rhs: TypedExpr[Type.Num, Γ], e: Env[Γ]) =
  interpret(lhs, e) + interpret(rhs, e)

private def runGt[Γ <: Tuple](lhs: TypedExpr[Type.Num, Γ], rhs: TypedExpr[Type.Num, Γ], e: Env[Γ]) =
  interpret(lhs, e) > interpret(rhs, e)

private def runCond[X <: Type, Γ <: Tuple](
    pred: TypedExpr[Type.Bool, Γ],
    onT: TypedExpr[X, Γ],
    onF: TypedExpr[X, Γ],
    e: Env[Γ]
) =
  if interpret(pred, e) then interpret(onT, e)
  else interpret(onF, e)

private def runLet[X <: Type, Y <: Type, Γ <: Tuple](
    value: TypedExpr[X, Γ],
    body: TypedExpr[Y, X *: Γ],
    e: Env[Γ]
): Value[Y] =
  interpret(body, e.push(interpret(value, e)))

private def runLetRec[X <: Type, Y <: Type, Γ <: Tuple](
    value: TypedExpr[X, X *: Γ],
    body: TypedExpr[Y, X *: Γ],
    e: Env[Γ]
): Value[Y] =
  val eʹ = e.push[X](null)
  val v  = interpret(value, eʹ)

  eʹ.value = v

  interpret(body, eʹ)

private def runRef[X <: Type, Γ <: NonEmptyTuple](loc: Elem[X, Γ], e: Env[Γ]): Value[X] =
  (e, loc) match
    case (Env.Cons(found, _), Elem.Here())     => found.nn
    case (Env.Cons(_, tail), Elem.There(rest)) => runRef(rest, tail)

private def runFun[X <: Type, Y <: Type, Γ <: Tuple](body: TypedExpr[Y, X *: Γ], e: Env[Γ]): Value[X -> Y] =
  x => interpret(body, e.push(x))

private def runApply[X <: Type, Y <: Type, Γ <: Tuple](fun: TypedExpr[X -> Y, Γ], arg: TypedExpr[X, Γ], e: Env[Γ]) =
  interpret(fun, e).apply(interpret(arg, e))

def interpret[X <: Type, Γ <: Tuple](
    expr: TypedExpr[X, Γ],
    e: Env[Γ]
): Value[X] =
  expr match
    case TypedExpr.Bool(value)          => value
    case TypedExpr.Num(value)           => value
    case TypedExpr.Add(lhs, rhs)        => runAdd(lhs, rhs, e)
    case TypedExpr.Gt(lhs, rhs)         => runGt(lhs, rhs, e)
    case TypedExpr.Cond(pred, onT, onF) => runCond(pred, onT, onF, e)
    case TypedExpr.Let(value, body)     => runLet(value, body, e)
    case TypedExpr.LetRec(value, body)  => runLetRec(value, body, e)
    case TypedExpr.Ref(loc)             => runRef(loc, e)
    case TypedExpr.Apply(fun, arg)      => runApply(fun, arg, e)

    // There's apparently an unhealthy interaction between GADTs and match types that make this cast necessary:
    // https://github.com/scala/scala3/issues/21391
    case TypedExpr.Fun(body) => runFun(body, e).asInstanceOf[Value[X]]
