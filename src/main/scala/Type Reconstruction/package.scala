import frontends.{ UntypedLC, AE, Conditionals }

package object typereconstruction extends TypeUtils {

  object Syntax extends UntypedLC with AE with Conditionals
  import Syntax._

  // additional base types
  case object IntT extends Type
  case object BooleanT extends Type

  /**
   * Typecheck the given tree. This is much nicer if encoded as attribute
   * grammar with one synthesized attribute per output.
   */
  def typecheck(e: Tree, env: Map[String, Type]): (Type, Constraints) = e match {

    // Interesting rules
    case Abs(name: String, body: Tree) => {
      // Make up a fresh type var and bind it to `name` in order to
      // type check the body
      val s = freshTypeVar
      typecheck(body, env + (name -> s)) match {
        case (t, c) => (ArrowT(s, t), c)
      }
    }
    case App(fun: Tree, arg: Tree) => (typecheck(fun, env), typecheck(arg, env)) match {
      case ((f, c1), (a, c2)) => {
        val x = freshTypeVar
        (x, (f =!= ArrowT(a, x)) :: (c1 ++ c2))
      }
    }

    // Boring rules
    case Var(n) => (env(n), Nil)
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