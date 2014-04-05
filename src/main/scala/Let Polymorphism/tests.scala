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
}