# sml-effect-conc

Algebraic effects based concurrency library in SML. Needs
[MLton](http://mlton.org/). Uses one-shot continuations
([MLton.Thread](http://mlton.org/MLtonThread)) to implement the concurrency
library.

## shallow-mono

Monomorphic shallow handlers where the argument and result type for
continuations/handlers are fixed.

## shallow-poly

Polymorphic shallow handlers using universal types. Requires [MLton with
serialization support](https://github.com/kayceesrk/mlton).
