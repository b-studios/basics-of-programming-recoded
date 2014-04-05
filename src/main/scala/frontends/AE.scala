package frontends

trait AE extends Trees {

  case class Num(value: Int) extends Tree
  case class Succ(base: Tree) extends Tree
  case class Pred(base: Tree) extends Tree

  implicit def intToNum(n: Int) = Num(n)

  implicit class AETreeOps(self: Tree) {
    def succ = Succ(self)
    def pred = Pred(self)
  }

}