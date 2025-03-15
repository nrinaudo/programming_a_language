package type_inference

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import Expr.*

class InferenceTests extends AnyFunSuite:

  def scrub(expr: Expr[Type]): Expr[Option[Type]] = expr match
    case Bool(value)                      => Bool(value)
    case Num(value)                       => Num(value)
    case Gt(lhs, rhs)                     => Gt(scrub(lhs), scrub(rhs))
    case Add(lhs, rhs)                    => Add(scrub(lhs), scrub(rhs))
    case Cond(pred, onT, onF)             => Cond(scrub(pred), scrub(onT), scrub(onF))
    case Let(name, value, vType, body)    => Let(name, scrub(value), None, scrub(body))
    case LetRec(name, value, vType, body) => LetRec(name, scrub(value), None, scrub(body))
    case Ref(name)                        => Ref(name)
    case Fun(param, pType, body)          => Fun(param, None, scrub(body))
    case Apply(fun, arg)                  => Apply(scrub(fun), scrub(arg))

  def runTest(expr: Expr[Type]) =
    infer(scrub(expr), TypeEnv.empty) match
      case Right(observed) => assert(observed == expr)
      case Left(e)         => fail(e)

  // let x = 1 in
  //   (let x = 2 in x) + x
  test("Bindings"):
    runTest(
      Let(
        name = "x",
        value = Num(1),
        vType = Type.Num,
        body = Add(
          Let(
            name = "x",
            vType = Type.Num,
            value = Num(2),
            body = Ref("x")
          ),
          Ref("x")
        )
      )
    )

  // let y = 1 in
  //   let f = (x: Num) -> x + y in
  //     let y = 2 in
  //       f 3
  test("Functions"):
    runTest(
      Let(
        name = "y",
        value = Num(1),
        vType = Type.Num,
        body = Let(
          name = "f",
          vType = Type.Num -> Type.Num,
          value = Fun(
            param = "x",
            pType = Type.Num,
            body = Add(Ref("x"), Ref("y"))
          ),
          body = Let(
            name = "y",
            value = Num(2),
            vType = Type.Num,
            body = Apply(Ref("f"), Num(3))
          )
        )
      )
    )

// let add = (lhs: Num) -> (rhs: Num) -> lhs + rhs in
  //   add 1 2
  test("Binary functions"):
    runTest(
      Let(
        name = "add",
        vType = Type.Num -> (Type.Num -> Type.Num),
        value = Fun(
          param = "lhs",
          pType = Type.Num,
          body = Fun(
            param = "rhs",
            pType = Type.Num,
            body = Add(Ref("lhs"), Ref("rhs"))
          )
        ),
        body = Apply(Apply(Ref("add"), Num(1)), Num(2))
      )
    )

  // let rec (sum : Num -> Num -> Num) = (lower: Num) -> (upper: Num) ->
  //   if lower > upper then 0
  //   else sum (lower + 1) upper
  test("Recursive functions"):
    runTest(
      LetRec(
        name = "sum",
        vType = Type.Num -> (Type.Num -> Type.Num),
        value = Fun(
          param = "lower",
          pType = Type.Num,
          body = Fun(
            param = "upper",
            pType = Type.Num,
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
      )
    )
