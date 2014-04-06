import frontends.{ UntypedLC, AE, Conditionals }
import utils._

package object letpolymorphism extends TypeUtils {

  object Syntax extends UntypedLC with frontends.Let with AE with Conditionals
  import Syntax._

  // additional base types
  case object IntT extends Type
  case object BooleanT extends Type


  /**
   * Typecheck the given tree. This is an extension to
   * `typereconstruction.typecheck`.
   *
   * Importantly we use Map[String, TypeScheme] instead of Map[String, Type]!
   */
  def typecheck(e: Tree, env: Map[String, TypeScheme]): (Type, Constraints) = e match {

    case Let(name, binding, body) => typecheck(binding, env) match {
      case (t, c) => {
        val τ = principleType(t, c)

        // Preventing capture of vars ∈ env prevents test program
        // `expectFailure` to typecheck.
        val principleTypeScheme = ∀(freeVars(τ) -- freeVars(env)) { τ }

        typecheck(body, env + (name -> principleTypeScheme)).mapRight(_ ++ c)
      }
    }


    // Also some changes for `Abs` and `Var`
    case Abs(name: String, body: Tree) => {
      val s = freshTypeVar
      typecheck(body, env + (name -> ∀(){ s })) match {
        case (t, c) => (ArrowT(s, t), c)
      }
    }
    case Var(n) => (instantiateTypeScheme(env(n)), Nil)

    // Nothing changed, compared to type reconstruction! 
    case App(fun: Tree, arg: Tree) => (typecheck(fun, env), typecheck(arg, env)) match {
      case ((f, c1), (a, c2)) => {
        val x = freshTypeVar
        (x, (f =!= ArrowT(a, x)) :: (c1 ++ c2))
      }
    }
    case Num(value: Int) => (IntT, Nil)
    case Bool(value: Boolean) => (BooleanT, Nil)
    case Succ(base: Tree) => typecheck(base, env) match {
      case (b, c) => (IntT, (b =!= IntT) :: c)
    }
    case Pred(base: Tree) => typecheck(base, env) match {
      case (b, c) => (IntT, (b =!= IntT) :: c)
    }
    case IsZero(base: Tree) => typecheck(base, env) match {
      case (b, c) => (BooleanT, (b =!= IntT) :: c)
    }
    case If(cond: Tree, thn: Tree, els: Tree) => 
      (typecheck(cond, env), typecheck(thn, env), typecheck(els, env)) match {
        case ((c, c1), (t, c2), (e, c3)) => 
          (t, (c =!= BooleanT) :: (t =!= e) :: (c1 ++ c2 ++ c3))
      }
  }

  def typeOf(e: Tree): Type = (principleType _).tupled(typecheck(e, Map.empty))

}