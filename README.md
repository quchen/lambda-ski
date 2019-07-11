Hello SK!
=========

Hello World, the most famous and basic computing problem! However, after
implementing it in several languages, I realized that this problem should be
solved once for many languages instead of having to be rewritten time and time
again. We clearly need a compiler for Hello World programs.

We start by writing what we believe to be Hello World in untyped lambda
calculus. Since modern operating systems lack an interpreter by default, we had
to write one here. Unfortunately, evaluating nominal [lambda calculus][λC] is
*hard*, so we simplify the problem by transforming it into [De Bruijn][deBruijn]
form, and then evaluate that. This allowed checking that the program is indeed
Hello World. We can finally compile this lambda calculus version to a
[SK][wikipedia-SK] based one, which is easy to evaluate in many practical
langauges, such as not Java.

You can view the generated Hello Worlds in the helloworld subdirectory.

[deBruijn]: https://en.wikipedia.org/wiki/De_Bruijn_index
[wikipedia-SK]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
[λC]: https://en.wikipedia.org/wiki/Lambda_calculus
