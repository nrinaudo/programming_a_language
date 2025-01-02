package typed_simple

import untyped.Expr
import typed_simple.{TypeEnv, TypeRepr}
import tests.InterpreterTests

class InterpretTests extends InterpreterTests:
  def run(expr: Expr) =
    val result = for
      typing    <- typecheck(expr, TypeEnv.empty)
      typedExpr <- typing.cast(TypeRepr.Num)
      value     <- interpret(typedExpr, Env.empty)
    yield value

    result match
      case Right(i)  => i
      case Left(err) => fail(err)
