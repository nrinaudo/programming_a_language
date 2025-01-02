package untyped

import tests.TypecheckerTests

class TypecheckTests extends TypecheckerTests:
  def run(expr: Expr) = typecheck(expr, TypeEnv.empty) match
    case Right(tpe) => tpe
    case _          => fail(s"Not an int: $value")
