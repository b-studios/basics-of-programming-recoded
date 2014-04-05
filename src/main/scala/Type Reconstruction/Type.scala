package typereconstruction

trait Type
case object IntT extends Type
case object BooleanT extends Type
case class ArrowT(from: Type, to: Type) extends Type
case class VarT(name: String) extends Type