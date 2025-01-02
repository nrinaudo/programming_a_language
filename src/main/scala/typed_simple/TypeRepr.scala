package typed_simple

import untyped.*

import untyped.{->, Type}

// - Type equality -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

  // We can easily compose two equalities. The compiler is now smart enough to realise that if `A` is the same as `B`,
  // and `B` the same as `C`, then surely `A` must be the same as `C`, and we can then produce a `Refl` for that.
  def andThen[C](eq: Eq[B, C]): Eq[A, C] = (this, eq) match
    case (Refl(), Refl()) => Refl()

  // The compiler knows that `A` and `B` are the same, so will be happy to treat `a` as a `B`.
  def cast(a: A): B = this match
    case Refl() => a

object Eq:
  // Lifts an equality into some `F`. This is what I couldn't achieve with `=:=`, because of the `<: Type` constraint.
  // Type constructor equality is injective, which the Scala 3 compiler has learned, which allows this to compile and
  // be sound.
  def injectivity[F[_ <: Type], A <: Type, B <: Type](
      eq: Eq[A, B]
  ): Eq[F[A], F[B]] = eq match
    case Refl() => Refl()

  // Injectivity, but for binary constructors.
  def injectivity2[F[_ <: Type, _ <: Type], LA <: Type, LB <: Type, RA <: Type, RB <: Type](
      eqA: Eq[LA, RA],
      eqB: Eq[LB, RB]
  ): (Eq[F[LA, LB], F[RA, RB]]) =
    // Eq[LA, RA] allows us to get the left hand side of our desired equality.
    val step1: Eq[F[LA, LB], F[RA, LB]] =
      injectivity[[X <: Type] =>> F[X, LB], LA, RA](eqA)

    // Eq[LB, RB] allows us to get the left hand side of our desired equality.
    val step2: Eq[F[RA, LB], F[RA, RB]] =
      injectivity[[X <: Type] =>> F[RA, X], LB, RB](eqB)

    // If you look at the types, we have the left hand side equal to some odd type, which is itself equal to the
    // right hand side. We can get our desired equality by transitivity of type equalities.
    step1.andThen(step2)

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
  case Num                                                           extends TypeRepr[Type.Num]
  case Bool                                                          extends TypeRepr[Type.Bool]
  case Fun[A <: Type, B <: Type](from: TypeRepr[A], to: TypeRepr[B]) extends TypeRepr[A -> B]

  // Attempts to cast an F[A] into an F[B].
  def cast[F[_ <: Type], B <: Type](fa: F[A], other: TypeRepr[B]): Either[String, F[B]] =
    (this =? other).map(Eq.injectivity[F, A, B]).map(_.cast(fa))

  // Attempts to produce a type quality between A and B.
  private def =?[B <: Type](to: TypeRepr[B]): Either[String, Eq[A, B]] =
    (this, to) match
      case (TypeRepr.Num, TypeRepr.Num)   => Right(Eq.Refl())
      case (TypeRepr.Bool, TypeRepr.Bool) => Right(Eq.Refl())
      case (left: TypeRepr.Fun[lf, lt], right: TypeRepr.Fun[rf, rt]) =>
        for
          from <- left.from =? right.from
          to   <- left.to =? right.to
        yield Eq.injectivity2[Type.Fun, lf, lt, rf, rt](from, to)

      case _ => Left(s"Failed to prove $this = $to")

object TypeRepr:
  // This gives us a `TypeRepr` for some `Type`. We don't know what the type parameter is, just that it exists, which
  // is enough for our purposes.
  def from(tpe: Type): TypeRepr[?] = tpe match
    case Type.Bool      => TypeRepr.Bool
    case Type.Num       => TypeRepr.Num
    case Type.Fun(a, b) => TypeRepr.Fun(TypeRepr.from(a), TypeRepr.from(b))
