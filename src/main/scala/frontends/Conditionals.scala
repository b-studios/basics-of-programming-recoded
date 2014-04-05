package frontends

trait Conditionals extends Trees { self: AE =>

  case class IsZero(base: Tree) extends Tree
  case class Bool(value: Boolean) extends Tree
  case class If(cond: Tree, thn: Tree, els: Tree) extends Tree 

  def _if(cond: Tree)(thn: Tree) = new {
    def _else(els: Tree) = If(cond, thn, els)
  }

  implicit def booleanToBool(b: Boolean) = Bool(b)

  implicit class ConditionalsTreeOps(self: Tree) {
    def isZero = IsZero(self)
  }

}