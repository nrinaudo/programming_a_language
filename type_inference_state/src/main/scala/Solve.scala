package type_inference

import Expr.*
import Inferrer.*

def substGt(lhs: Expr[TypeInf], rhs: Expr[TypeInf], s: InfState) =
  for
    lhs <- subst(lhs, s)
    rhs <- subst(rhs, s)
  yield Gt(lhs, rhs)

def substAdd(lhs: Expr[TypeInf], rhs: Expr[TypeInf], s: InfState) =
  for
    lhs <- subst(lhs, s)
    rhs <- subst(rhs, s)
  yield Add(lhs, rhs)

def substCond(pred: Expr[TypeInf], onT: Expr[TypeInf], onF: Expr[TypeInf], s: InfState) =
  for
    pred <- subst(pred, s)
    onT  <- subst(onT, s)
    onF  <- subst(onF, s)
  yield Cond(pred, onT, onF)

def substLet(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    value <- subst(value, s)
    x     <- getType(x, s)
    body  <- subst(body, s)
  yield Let(name, value, x, body)

def substFun(param: String, x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    x    <- getType(x, s)
    body <- subst(body, s)
  yield Fun(param, x, body)

def substApply(fun: Expr[TypeInf], arg: Expr[TypeInf], s: InfState) =
  for
    arg <- subst(arg, s)
    fun <- subst(fun, s)
  yield Apply(fun, arg)

def substLetRec(name: String, value: Expr[TypeInf], x: TypeInf, body: Expr[TypeInf], s: InfState) =
  for
    value <- subst(value, s)
    x     <- getType(x, s)
    body  <- subst(body, s)
  yield LetRec(name, value, x, body)

def substRef(name: String, s: InfState): Either[String, Expr[Type]] = Right(Ref(name))

def getType(t: TypeInf, s: InfState): Either[String, Type] = t match
  case TypeInf.Num  => Right(Type.Num)
  case TypeInf.Bool => Right(Type.Bool)
  case TypeInf.Fun(x, y) =>
    for
      x <- getType(x, s)
      y <- getType(y, s)
    yield Type.Fun(x, y)
  case TypeInf.Var(i) => s.get(i).toRight(s"Variable not resolved: $i").flatMap(getType(_, s))

def subst(expr: Expr[TypeInf], s: InfState): Either[String, Expr[Type]] =
  expr match
    case Bool(value)                      => Right(Bool(value))
    case Num(value)                       => Right(Num(value))
    case Gt(lhs, rhs)                     => substGt(lhs, rhs, s)
    case Add(lhs, rhs)                    => substAdd(lhs, rhs, s)
    case Cond(pred, onT, onF)             => substCond(pred, onT, onF, s)
    case Let(name, value, vType, body)    => substLet(name, value, vType, body, s)
    case LetRec(name, value, vType, body) => substLetRec(name, value, vType, body, s)
    case Ref(name)                        => substRef(name, s)
    case Fun(param, pType, body)          => substFun(param, pType, body, s)
    case Apply(fun, arg)                  => substApply(fun, arg, s)
