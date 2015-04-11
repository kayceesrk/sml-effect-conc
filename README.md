# sml-effect-conc

Algebraic effects based concurrency library in SML. Needs
[MLton](http://mlton.org/). Uses one-shot continuations
([MLton.Thread](http://mlton.org/MLtonThread)) to implement the concurrency
library. The are monomorphic and polymorphic extensions of [deep and shallow
handlers](http://homepages.inf.ed.ac.uk/slindley/papers/handlers.pdf). Each
variant is illustrated with a simple cooperative concurrency library.

In the monomorphic API, the argument and return type of continuations and
handlers are fixed. The polymorphic versions use universal types. Universal
types are used to enumlate polymorphic thread-local storage, absent in
MLton.Thread library, using a global reference. Particularly, the universal
type implementation is "unsafe" (core-dumps on incorrect cast) as opposed to
["safe" universal types](http://mlton.org/UniversalType); unsafe variant is
necessary to maintain the polymorphism of global reference (value restriction?)
(*But is this really necessary?*). However, the use of universal types is
completely hidden within the library implementation, and is safe. The unsafe
universal types are implemented using serialization and requires [MLton with
serialization support](https://github.com/kayceesrk/mlton).

The polymorphic functions carry extra type variables when compared to the OCaml
signatures since Standard ML does not have existential types to abstract the
type variables.
