package typed_env

import untyped.Expr
import tests.InterpreterTests

class InterpretTests extends InterpreterTests:
  override def run(expr: Expr) =
    val result = for
      typing    <- typeCheck(expr, TypeEnv.Empty)
      typedExpr <- typing.cast(TypeRepr.Num)
    yield interpret(typedExpr.expr, Env.Empty)

    result match
      case Right(i)  => i
      case Left(err) => fail(err)
