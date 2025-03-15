package type_inference

enum Expr[T]:
  case Bool(value: Boolean)
  case Num(value: Int)
  case Gt(lhs: Expr[T], rhs: Expr[T])
  case Add(lhs: Expr[T], rhs: Expr[T])
  case Cond(pred: Expr[T], onT: Expr[T], onF: Expr[T])
  case Let(name: String, value: Expr[T], vType: T, body: Expr[T])
  case LetRec(name: String, value: Expr[T], vType: T, body: Expr[T])
  case Ref(name: String)
  case Fun(param: String, pType: T, body: Expr[T])
  case Apply(fun: Expr[T], arg: Expr[T])
