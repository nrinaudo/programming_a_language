package tests

import untyped.{Expr, Type}
import untyped.Expr.*

object TestCase:
  // 1 + 2 + 3
  val basicAst = Add(Num(1), Add(Num(2), Num(3)))

  // if true then 1
  // else         2
  val conditionals = Cond(
    pred = Bool(true),
    onT = Num(1),
    onF = Num(2)
  )

  // let x = 1 in
  //   (let x = 2 in x) + x
  val bindings = Let(
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
  )

  // let y = 1 in
  //   let f = (x: Num) -> x + y in
  //     let y = 2 in
  //       f 3
  val functions = Let(
    name = "y",
    value = Num(1),
    body = Let(
      name = "f",
      value = Fun(
        param = "x",
        pType = Type.Num,
        body = Add(Ref("x"), Ref("y"))
      ),
      body = Let(
        name = "y",
        value = Num(2),
        body = Apply(Ref("f"), Num(3))
      )
    )
  )

  // let add = (lhs: Num) -> (rhs: Num) -> lhs + rhs in
  //   add 1 2
  val binaryFunctions = Let(
    name = "add",
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

  // let rec (sum : Num -> Num -> Num) = (lower: Num) -> (upper: Num) ->
  //   if lower > upper then 0
  //   else sum (lower + 1) upper
  val recursion = LetRec(
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
