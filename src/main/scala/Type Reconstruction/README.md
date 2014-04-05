Type Reconstruction
===================

_Based on Pierce PLT, ch. 22 "Type Reconstruction"._

Language: LC (Without type ascriptions) + arithm. expression + isZero + If

Key Features: Full reconstruction of type annotations.

Description of the Algorithm
----------------------------
It follows three simple steps:

1. Collect constraints
2. Unify constraints
3. Use unifier σ to substitute types in result

While type checking, constraints are collected. Instead of immediately checking,
that the function position is of type `S => T` a new type variable `X` is
generated and returned. In addition the constraint `X =!= ArrowT(S, T)` is
added.

For type variables the Barendregt convention is used. Every occurrence is assigned
a new type variable `?X_i`.

After collecting the constraints, those are unified using standard unification.
A variable is substituted by its definition. 

    VarT(x) =!= T

This substitution `Var(x) -> T` is recorded as principle (most general) unifier
σ.

Type checking returns (τ, C). We compute σ to gain as result (τ, σ) called the
principle solution. (It has to be the smallest solution such that for every other
solution σ' we have σ ⊑ σ')

THe overall result type after substitution is: σ(τ)