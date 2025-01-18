package typed_env

import untyped.{->, Expr, Type}
import typed_simple.TypeRepr

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
