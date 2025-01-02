package untyped

enum Type:
  case Num
  case Bool
  case Fun[X <: Type, Y <: Type](from: X, to: Y)

  def ->(other: Type): Type = Fun(this, other)

type ->[X <: Type, Y <: Type] = Type.Fun[X, Y]

object Type:
  type Num  = Type.Num.type
  type Bool = Type.Bool.type
