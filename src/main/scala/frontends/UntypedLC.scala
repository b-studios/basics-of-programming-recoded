package frontends

trait UntypedLC extends Trees {

  case class Var(name: String) extends Tree
  case class Abs(name: String, body: Tree) extends Tree
  case class App(fun: Tree, arg: Tree) extends Tree

  implicit def stringToVar(n: String): Tree = Var(n)
  def λ (name: String)(body: Tree => Tree) = Abs(name, body(Var(name)))
  def lam(name: String)(body: Tree => Tree) = Abs(name, body(Var(name)))

  implicit class UnTypedLCTreeOps(self: Tree) {
    def apply(other: Tree) = App(self, other)
  }
}