package type_inference

import Expr.*

enum TypeInf:
  case Num
  case Bool
  case Fun(from: TypeInf, to: TypeInf)
  case Var(index: Int)
  def ->(t: TypeInf): TypeInf = Fun(this, t)

object TypeInf:
  def from(tpe: Type): TypeInf = tpe match
    case Type.Num       => TypeInf.Num
    case Type.Bool      => TypeInf.Bool
    case Type.Fun(a, b) => from(a) -> from(b)
