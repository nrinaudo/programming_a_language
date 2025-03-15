package type_inference

def infer(expr: Expr[Option[Type]], Γ: TypeEnv): Either[String, Expr[Type]] =
  for
    Inferrer.Result(state, Typing(expr, _)) <- typeCheck(expr, Γ).run(InfState.empty)
    expr                                    <- subst(expr, state)
  yield expr
