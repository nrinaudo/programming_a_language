package untyped

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import Expr.*

class InterpretTests extends AnyFunSuite:
  def runTest(expr: Expr, expected: Value) =
    interpret(expr, Env.empty) shouldBe expected

  // 1 + (2 + 3)
  test("Addition"):
    runTest(
      expr = Add(Num(1), Add(Num(2), Num(3))),
      expected = Value.Num(6)
    )

  // if true then 1
  // else         2
  test("Conditional"):
    runTest(
      expr = Cond(
        pred = Bool(true),
        onT = Num(1),
        onF = Num(2)
      ),
      expected = Value.Num(1)
    )

  // let x = 1 in
  //   (let x = 2 in x) + x
  test("Binding"):
    runTest(
      expr = Let(
        name = "x",
        value = Num(1),
        body = Add(
          Let(
            name = "x",
            value = Num(2),
            body = Ref("x")
          ),
          Ref("x")
        )
      ),
      expected = Value.Num(3)
    )

  // let y = 1 in
  //   let f = (x: Num) -> x + y in
  //     let y = 2 in
  //       f 3
  test("Function"):
    runTest(
      expr = Let(
        name = "y",
        value = Num(1),
        body = Let(
          name = "f",
          value = Fun(
            param = "x",
            body = Add(Ref("x"), Ref("y"))
          ),
          body = Let(
            name = "y",
            value = Num(2),
            body = Apply(Ref("f"), Num(3))
          )
        )
      ),
      expected = Value.Num(4)
    )

  // let add = (lhs: Num) -> (rhs: Num) -> lhs + rhs in
  //   add 1 2
  test("Binary function"):
    runTest(
      expr = Let(
        name = "add",
        value = Fun(
          param = "lhs",
          body = Fun(
            param = "rhs",
            body = Add(Ref("lhs"), Ref("rhs"))
          )
        ),
        body = Apply(Apply(Ref("add"), Num(1)), Num(2))
      ),
      expected = Value.Num(3)
    )

  // let rec (sum : Num -> Num -> Num) = (lower: Num) -> (upper: Num) ->
  //   if lower > upper then 0
  //   else sum (lower + 1) upper
  test("Recursive function"):
    runTest(
      expr = LetRec(
        name = "sum",
        value = Fun(
          param = "lower",
          body = Fun(
            param = "upper",
            body = Cond(
              pred = Gt(Ref("lower"), Ref("upper")),
              onT = Num(0),
              onF = Add(
                lhs = Ref("lower"),
                rhs = Apply(Apply(Ref("sum"), Add(Ref("lower"), Num(1))), Ref("upper"))
              )
            )
          )
        ),
        body = Apply(Apply(Ref("sum"), Num(1)), Num(10))
      ),
      expected = Value.Num(55)
    )
