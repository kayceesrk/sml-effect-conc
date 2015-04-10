# sml-effect-conc

Algebraic effects based concurrency library in SML. Needs
[MLton](http://mlton.org/). Uses one-shot continuations
([MLton.Thread](http://mlton.org/MLtonThread)) to implement the concurrency
library. The API is mostly monomorphic given the lack of advanced type system
support in SML (GADTs, existentials, etc).

## Shallow

Implements shallow handlers.
