package typed_ast

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
      name: String,
      value: TypedExpr[A],
      vType: TypeRepr[A],
      body: TypedExpr[B]
  ) extends TypedExpr[B]
  case LetRec[A <: Type, B <: Type](
      name: String,
      value: TypedExpr[A],
      vType: TypeRepr[A],
      body: TypedExpr[B]
  ) extends TypedExpr[B]
  case Ref[A <: Type](
      name: String,
      rType: TypeRepr[A]
  ) extends TypedExpr[A]
  case Fun[A <: Type, B <: Type](
      param: String,
      pType: TypeRepr[A],
      body: TypedExpr[B]
  ) extends TypedExpr[A -> B]
  case Apply[A <: Type, B <: Type](
      fun: TypedExpr[A -> B],
      arg: TypedExpr[A]
  ) extends TypedExpr[B]
