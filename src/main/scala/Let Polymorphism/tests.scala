package letpolymorphism

object tests {

  import Syntax._

  val prog: Tree = 
    let("dbl", λ("f") { f => λ("x") { x => f(f(x)) } }) { dbl =>
      let("dblInt", dbl(λ("x"){ _.succ })) { dblInt => 
        let("dblBool", dbl(λ("x"){ _ => false })) { dblBool => 
          _if(dblBool(true)) { dblInt(1) } _else { dblInt(2) }
        }
      }
    }

  println(typeOf(prog))

  val expectFailure: Tree = 
    ((λ("f") { f =>
      λ("x") { x => let("g", f) { _(0) } }
    }) (λ("x") { x => _if (x) { true } _else { false} })) (true)

  println(typeOf(expectFailure)) 
  //=> Fails, cannot unify IntT with BooleanT
}