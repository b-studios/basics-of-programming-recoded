package typereconstruction

object tests {

  import Syntax._
  
  println(typeOf(Num(4).succ.pred.succ.isZero)) //=> BooleanT
  println(typeOf(λ("x") { _.succ })) //=> (IntT => IntT)
  
  val prog: Tree = 
    λ("x") { x =>
      λ("y") { y =>
        x (y)
      }
    }(λ("x") { x =>
      x.succ
    })

  println(typeOf(prog)) //=> (IntT => IntT)

  typeOf(λ("x") { x => x(x) })
  //=> Cannot unify VarT(?x_7) with ArrowT(VarT(?x_7),VarT(?x_8)), List()
}