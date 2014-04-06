Let Polymorphism
================

_Based on: section 22.7 "Let-Polymorphism" in Pierce PLT_

Language: LC (without type ascription) + Let + AE + IsZero + If

Key Idea: Instantiate a polymorphic function, once per occurrence.

    let dbl = λf.λx.f(f x) in
      let dblInt  = dbl λn. (succ n) in
      let dblBool = dbl λb. (not b) in
        ...

    f :: ∀T. (T → T) → T

    T =!= Int
    T =!= Boolean

Trying to unify all constraints imposed by the usage of the bound value leads to
a contradiction `Int =:= Boolean`.

So we need to instantiate the bound value *once per usage*.

> We have to perform one single step of evaluation (substitute the binding for
> typechecking)