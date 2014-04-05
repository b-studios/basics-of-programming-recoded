package typereconstruction

trait TypeUtils {

  trait Type
  case class ArrowT(from: Type, to: Type) extends Type
  case class VarT(name: String) extends Type

  /**
   * Creates globally fresh type variables
   */
  private var _count = 0
  def freshName = {
    _count += 1
    "?x_" + _count
  }
  def freshTypeVar = VarT(freshName)


  /**
   * Constraint that left and right should be equal
   */
  case class Constraint(left: Type, right: Type)
  type Constraints = List[Constraint]
  implicit class TypeConstraint(self: Type) {
    def =!=(other: Type) = Constraint(self, other)
  }
  object =!= {
    def unapply(c: Constraint): Option[(Type, Type)] =
      Some((c.left, c.right))
  }


  /**
   * A Unifier is a substitution from VarT -> Type
   */
  type Unifier = Map[VarT, Type]
  type UnificationResult = Either[String, Unifier]

  def unify(cs: Constraints): UnificationResult = cs.distinct match {
    case Nil => Right(Map.empty)

    case (s =!= t) :: rest if s == t => unify { rest }

    case ((s: VarT) =!= t) :: rest if !(s occursIn t) =>
      unify { 
        substitute(s, t, rest) 
      }.right.map(_ + (s -> t))
    
    case (s =!= (t: VarT)) :: rest if !(t occursIn s) =>
      unify { 
        substitute(t, s, rest) 
      }.right.map(_ + (t -> s))

    case (ArrowT(f1, a1) =!= ArrowT(f2, a2)) :: rest =>
      unify { rest ++ List(f1 =!= f2, a1 =!= a2) }

    case (s =!= t) :: rest => Left(s"Cannot unify $s with $t, $rest")
  }

  /** 
   * The "occurs" check. Returns true if x occurs in y
   */
  def occurs(x: VarT, y: Type): Boolean = y match {
    case `x` => true
    case ArrowT(f, a) => occurs(x, f) || occurs(x, a)
    case _ => false
  }
  implicit class VarTOps[T <: Type](self: T) {
    def occursIn(other: Type)(implicit ev: T <:< VarT) = 
      occurs(ev(self), other)
  }

  /** 
   * Type substitution
   */
  def substitute(x: VarT, by: Type, in: Type): Type = in match {
    case `x` => by
    case ArrowT(f, a) => ArrowT(substitute(x, by, f), substitute(x, by, a))
    case other => other
  }
  def substitute(x: VarT, by: Type, in: Constraints): Constraints = in.map {
    case l =!= r => substitute(x, by, l) =!= substitute(x, by, r)
  }
  def substitute(t: Type, unifier: Unifier): Type = t match {
    case x: VarT => unifier.get(x).map(substitute(_, unifier)).getOrElse(x)
    case ArrowT(f, a) => ArrowT(substitute(f, unifier), substitute(a, unifier))
    case other => other
  }

  def principleType(t: Type, cs: Constraints): Type = {
    val unifier = unify(cs) match {
      case Left(msg) => sys error msg
      case Right(u) => u
    }
    substitute(t, unifier)
  }

}