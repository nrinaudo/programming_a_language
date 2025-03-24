package typed_env

import TypedExpr.*

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

// - Indexed types ----------------------------------------------------------------------------------------------------
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

// - Typed AST ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Proof that X is in XS, as well as way to get it.
// - `Here[X, XS]` tells us that we have an `X` at the head of an `X *: XS`.
// - `There[H, X, XS]` tells us that `X` will be found somewhere in `XS`.
enum Elem[X, XS <: NonEmptyTuple]:
  case Here[X, XS <: Tuple]()                              extends Elem[X, X *: XS]
  case There[H, X, XS <: NonEmptyTuple](elem: Elem[X, XS]) extends Elem[X, H *: XS]

// Typed AST, encoding both the type of the value it will evaluate to and the environment in which it must be
// interpreted.
// That environment is expressed as a tuple, seen as a stack of types.
enum TypedExpr[X <: Type, Γ <: Tuple]:
  // Γ |- Bool value : Type.Bool
  case Bool[Γ <: Tuple](value: Boolean) extends TypedExpr[Type.Bool, Γ]

  // Γ |- Num value : Type.Num
  case Num[Γ <: Tuple](value: Int) extends TypedExpr[Type.Num, Γ]

  // Γ |- lhs : Type.Num       Γ |- rhs : Type.Num
  // ---------------------------------------------
  //       Γ |- Add lhs rhs : Type.Num
  case Add[Γ <: Tuple](
      lhs: TypedExpr[Type.Num, Γ],
      rhs: TypedExpr[Type.Num, Γ]
  ) extends TypedExpr[Type.Num, Γ]

  // Γ |- lhs : Type.Num      Γ |- rhs : Type.Num
  // --------------------------------------------
  //      Γ |- Gt lhs rhs : Type.Bool
  case Gt[Γ <: Tuple](
      lhs: TypedExpr[Type.Num, Γ],
      rhs: TypedExpr[Type.Num, Γ]
  ) extends TypedExpr[Type.Bool, Γ]

  // Γ |- pred : Type.Bool      Γ |- onT : X       Γ |- onF : X
  // ----------------------------------------------------------
  //          Γ |- Cond pred onT onF : X
  case Cond[X <: Type, Γ <: Tuple](
      pred: TypedExpr[Type.Bool, Γ],
      onT: TypedExpr[X, Γ],
      onF: TypedExpr[X, Γ]
  ) extends TypedExpr[X, Γ]

  // Γ |- value : X       Γ[name <- X] |- body : Y
  // ---------------------------------------------
  //       Γ |- Let name value body : Y
  case Let[X <: Type, Y <: Type, Γ <: Tuple](
      value: TypedExpr[X, Γ],
      body: TypedExpr[Y, X *: Γ]
  ) extends TypedExpr[Y, Γ]

  // Γ[name <- X] |- value : X      Γ[name <- X] |- body : Y
  // -------------------------------------------------------
  //       Γ |- LetRec name value body : Y
  case LetRec[X <: Type, Y <: Type, Γ <: Tuple](
      value: TypedExpr[X, X *: Γ],
      body: TypedExpr[Y, X *: Γ]
  ) extends TypedExpr[Y, Γ]

  // ​Γ |- Ref name : Γ(name)
  case Ref[X <: Type, Γ <: NonEmptyTuple](path: Elem[X, Γ]) extends TypedExpr[X, Γ]

  //     Γ[param <- X] |- body : Y
  // ----------------------------------
  // Γ |- Fun (param : X) body : X -> Y
  case Fun[X <: Type, Y <: Type, Γ <: Tuple](
      body: TypedExpr[Y, X *: Γ]
  ) extends TypedExpr[X -> Y, Γ]

  // Γ |- fun : X -> Y     Γ |- arg : X
  // ----------------------------------
  //      Γ |- Apply fun arg : Y
  case Apply[X <: Type, Y <: Type, Γ <: Tuple](
      fun: TypedExpr[X -> Y, Γ],
      arg: TypedExpr[X, Γ]
  ) extends TypedExpr[Y, Γ]

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
