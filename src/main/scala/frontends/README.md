Frontends
=========

These are the language frontend components, that can be composed to form the
syntactic basis of language implementations.

For a lambda calculus with arithmetic expressions just use:

~~~scala
object Syntax extends UntypedLC with AE
import Syntax._

λ("x") { _.succ } // yields: Abs("x", Succ(Var("x")))
~~~

Available languages
-------------------

- untyped λ-calculus
- let-extension for untyped λ-calculus
- arithmetic expressions (including num, succ, pred)
- conditionals (including iszero, if, boolean)