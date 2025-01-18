package typed_simple

import untyped.*

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

// - Indexed types -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Type checking will require us to be able to associate an `Expr[A]` with a `Type`, and to guarantee that `A` and the
// `Type` match.
// We achieve this with GADTs, by using `TypeRepr[A]` (which is really just a different view of `Type`, with a type
// parameter).
//
// This way, if we manage to stick a `TypeRepr[A]` and an `Expr[A]` in the same branch of the pattern match, we can
// match on the `TypeRepr` being `TypeRepr.Bool`, for example, which guarantees that the expression is in fact an
// `Expr[Type.Bool]`.
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
