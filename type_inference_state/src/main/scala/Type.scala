package type_inference

enum Type:
  case Num
  case Bool
  case Fun[X <: Type, Y <: Type](from: X, to: Y)
  def ->(t: Type): Type = Fun(this, t)

object Type:
  type Num  = Type.Num.type
  type Bool = Type.Bool.type
