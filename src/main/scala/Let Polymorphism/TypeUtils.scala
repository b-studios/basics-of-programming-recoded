package letpolymorphism

trait TypeUtils extends typereconstruction.TypeUtils {

  /**
   * Type schemas bind free variables by universal quantification
   * ∀X_1, ... X_2. T
   */
  case class TypeScheme(vars: List[VarT], body: Type)
  object ∀ {
    def apply(vars: VarT*)(body: Type) = TypeScheme(vars.toList, body)
    def apply(vars: List[VarT])(body: Type) = TypeScheme(vars, body)
  }


  def instantiateTypeScheme(ts: TypeScheme): Type = ts match {
    case TypeScheme(xs, t) => {
      val σ = xs.map { x: VarT => (x, freshTypeVar) }.toMap
      substitute(t, σ)
    }
  }

  def freeVars(t: Type): Set[String] = t match {
    case VarT(n) => Set(n)
    case ArrowT(l, r) => freeVars(l) ++ freeVars(r)
    case _ => Set.empty
  }

}