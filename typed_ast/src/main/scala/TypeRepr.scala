package typed_ast

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
