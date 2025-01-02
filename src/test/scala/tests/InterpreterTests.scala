package tests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import untyped.Expr

abstract class InterpreterTests extends AnyFunSuite with Matchers:
  def run(expr: Expr): Int

  def runTest(expr: Expr, expected: Int) =
    run(expr) should be(expected)

  test("Basic AST"):
    runTest(TestCase.basicAst, 6)

  test("Conditionals"):
    runTest(TestCase.conditionals, 1)

  test("Bindings"):
    runTest(TestCase.bindings, 3)

  test("Functions"):
    runTest(TestCase.functions, 4)

  test("Binary functions"):
    runTest(TestCase.binaryFunctions, 3)

  test("Recursion"):
    runTest(TestCase.recursion, 55)
