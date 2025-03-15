package typed_ast

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
