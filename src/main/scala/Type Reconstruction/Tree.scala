package typereconstruction

trait Tree

case class Var(name: String) extends Tree
case class Abs(name: String, body: Tree) extends Tree
case class App(fun: Tree, arg: Tree) extends Tree
case class Num(value: Int) extends Tree
case class Succ(base: Tree) extends Tree
case class Pred(base: Tree) extends Tree
case class IsZero(base: Tree) extends Tree
case class Bool(value: Boolean) extends Tree
case class If(cond: Tree, thn: Tree, els: Tree) extends Tree 


object edsl {

  // Lambda Abstractions
  implicit def stringToVar(n: String) = Var(n)
  def λ (name: String)(body: Tree => Tree) = Abs(name, body(Var(name)))
  def lam(name: String)(body: Tree => Tree) = Abs(name, body(Var(name)))

  // Numbers
  implicit def intToNum(n: Int) = Num(n)

  // Boolean
  implicit def booleanToBool(b: Boolean) = Bool(b)

  def _if(cond: Tree)(thn: Tree) = new {
    def _else(els: Tree) = If(cond, thn, els)
  }

  implicit class TreeOps(self: Tree) {
    def apply(other: Tree) = App(self, other)
    def succ = Succ(self)
    def pred = Pred(self)
    def isZero = IsZero(self)
  }

}