package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import untyped.{Expr, Type}
import untyped.Expr.*

abstract class TypecheckerTests extends AnyFunSuite with Matchers:
  def run(expr: Expr): Type

  def runTest(expr: Expr, expected: Type) =
    run(expr) should be(expected)

  test("Basic AST"):
    runTest(TestCase.basicAst, Type.Num)

  test("Conditionals"):
    runTest(TestCase.conditionals, Type.Num)

  test("Bindings"):
    runTest(TestCase.bindings, Type.Num)

  test("Functions"):
    runTest(TestCase.functions, Type.Num)

  test("Binary functions"):
    runTest(TestCase.binaryFunctions, Type.Num)

  test("Recursion"):
    runTest(TestCase.recursion, Type.Num)

  test("Boolean"):
    runTest(Bool(true), Type.Bool)

  test("Number"):
    runTest(Num(1), Type.Num)

  test("Function"):
    runTest(Fun("x", Type.Num, Add(Ref("x"), Num(1))), Type.Num -> Type.Num)
