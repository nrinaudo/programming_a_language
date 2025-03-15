package untyped

enum Expr:
  case Bool(value: Boolean)
  case Num(value: Int)
  case Gt(lhs: Expr, rhs: Expr)
  case Add(lhs: Expr, rhs: Expr)
  case Cond(pred: Expr, onT: Expr, onF: Expr)
  case Let(name: String, value: Expr, body: Expr)
  case LetRec(name: String, value: Expr, body: Expr)
  case Ref(name: String)
  case Fun(param: String, body: Expr)
  case Apply(fun: Expr, arg: Expr)
