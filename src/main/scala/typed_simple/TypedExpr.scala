package typed_simple

import untyped.*

// - Typed expressions -------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Stores the name and type of a variable.
// It might seem counter-intuitive that we need to store type information in a type-checked representation: after all
// we've already checked everything, so why bother carry this additional information?
//
// It turns out to be necessary when actually interpreting a type-checked expression: whenever interacting with
// variables, we need to make sure that the value we get is of the expected type. This can only be done if we know
// what the expected type is at runtime.
case class Variable[T <: Type](name: String, tpe: TypeRepr[T])

// Type-checked version of `Expr`: this guarantees that all operations are type-correct. It is now impossible to
// write a conditional statement that returns a different type depending on the branch it takes, or to add ints and
// booleans.
// `Expr` is indexed on something that must be a `Type`: this tells us what `Type` an `Expr` evaluates to.
//
// You'll note that `Expr` has essentially the same variants as `Expr`, with better type constraints on sub-
// expressions.
//
// This, unfortunately, leads to some rather unpleasant type signatures, where just about everything needs to be
// `<: Type`.
enum TypedExpr[A <: Type]:
  case Bool(value: Boolean) extends TypedExpr[Type.Bool]
  case Num(value: Int)      extends TypedExpr[Type.Num]
  case Gt(
      lhs: TypedExpr[Type.Num],
      rhs: TypedExpr[Type.Num]
  ) extends TypedExpr[Type.Bool]
  case Add(
      lhs: TypedExpr[Type.Num],
      rhs: TypedExpr[Type.Num]
  ) extends TypedExpr[Type.Num]
  case Cond[A <: Type](
      pred: TypedExpr[Type.Bool],
      onT: TypedExpr[A],
      onF: TypedExpr[A]
  ) extends TypedExpr[A]
  case Let[A <: Type, B <: Type](
      variable: Variable[A],
      value: TypedExpr[A],
      body: TypedExpr[B]
  ) extends TypedExpr[B]
  case LetRec[A <: Type, B <: Type](
      variable: Variable[A],
      value: TypedExpr[A],
      body: TypedExpr[B]
  ) extends TypedExpr[B]
  case Ref[A <: Type](
      variable: Variable[A]
  ) extends TypedExpr[A]
  case Fun[A <: Type, B <: Type](
      param: Variable[A],
      body: TypedExpr[B]
  ) extends TypedExpr[A -> B]
  case Apply[A <: Type, B <: Type](
      fun: TypedExpr[A -> B],
      arg: TypedExpr[A]
  ) extends TypedExpr[B]
