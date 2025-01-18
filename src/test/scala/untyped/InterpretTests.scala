package untyped

import tests.InterpreterTests

class InterpretTests extends InterpreterTests:
  def run(expr: Expr) = interpret(expr, Env.empty) match
    case Value.Num(i) => i
    case _            => fail(s"Not an int: $expr")
