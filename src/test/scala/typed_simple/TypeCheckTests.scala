package typed_simple

import untyped.{Expr, Type}
import tests.TypeCheckerTests

class TypeCheckTests extends TypeCheckerTests:
  def getType(repr: TypeRepr[?]): Type = repr match
    case TypeRepr.Num           => Type.Num
    case TypeRepr.Bool          => Type.Bool
    case TypeRepr.Fun(from, to) => Type.Fun(getType(from), getType(to))

  def run(expr: Expr) =
    typeCheck(expr, TypeEnv.empty) match
      case Right(typing) => getType(typing.repr)
      case Left(err)     => fail(err)
