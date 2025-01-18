package untyped

import tests.TypeCheckerTests

class TypeCheckTests extends TypeCheckerTests:
  def run(expr: Expr) = typeCheck(expr, TypeEnv.empty) match
    case Right(tpe) => tpe
    case _          => fail(s"Not an int: $expr")
