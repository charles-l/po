# po
minimal lisp implementation

# Goals
<small><small>Heh. These keep changing.</small></small>

* Compile to C (inspired by chickenscheme/crystal) - I've decided that that's the easiest way to work with C FFI then ;)
* Make it easy to use (like ruby)
    * This is vague, but it basically means to be somewhat duck-typey(?) the `+` function should do what you expect whether you pass it a number, a string or a list/vector.
    * Terse syntax. Because long function names are stupid.
    * Consistent syntax. Don't %{go} ([the way of]) '(the) #([{clojure}]).
* Make it fun.
    * Once again, steal from ruby:
        * Make a solid, functional-ish stdlib.
        * Remove boilerplate.
* Be more lispy than scheme-y
    * Stick to lisps minimalistic roots.
    * Don't make things confusing like chicken-scheme though. There shouldn't be 13 slightly different ways to read a file, 16 different macro functions or 32 different error catching/throwing methods.
* Leverage the power of C
    * Make it easy and seamless to drop into C to do work there 'cause C really rocks for some things.
    * Make FFIs work easily.
    * Make Lisp lists easy to walk through in C.

## What's implemented
* really basic lisp
* requires chicken-scheme to interpret
* example:
    * `(set! fact (lam (n) (if (= n 1) 1 (* n (fact (- n 1))))))`
    * `(p (fact 10))`
* dynamic variable creation doesn't exist yet (the `fact` binding is defined at compile time)
