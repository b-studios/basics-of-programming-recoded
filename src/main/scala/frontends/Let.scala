package frontends

trait Let extends Trees { self: UntypedLC =>

  case class Let(name: String, binding: Tree, body: Tree) extends Tree

  def let(name: String, binding: Tree)(body: Tree => Tree) =
    Let(name, binding, body(Var(name)))
}