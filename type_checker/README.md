# About

Ressources:
* the type checker of [roc-lang](https://github.com/roc-lang/roc/blob/22423ca98bef41e2f55ae54e11c632bd0610c241/crates/compiler/solve/src/solve.rs)
* the 3rd chapter of [static program analysis](https://cs.au.dk/~amoeller/spa/)

The type checker does 2 main passes:

1. firts it collects all the type constraints in can see, 

for instance, if `var a = 1 + b;` then we know that:
* `a` as the same type as the expression `1 + b`
* `1 + b` is either a string of a number (only types that supports addition)
* `1 + b` has the same type as `1` (because addition preserves type)
* `1 + b` has the same type as `b` (same)
* `1`  as the same type as `b` (because we can only add 2 strings, or 2 numbers together)
* `1` is a number

2. then, to actually solve those constraints, my current plan is to use the W algorithm (Hindley-Milner),
but I'm not entirely sure that it can be done.
