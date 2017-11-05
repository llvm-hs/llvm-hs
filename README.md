IRBuilder
=========

A IRBuilder, starting out as a thin reinterpretation of the C++ IRBuilder inside
of a Haskell State monad. Goal is to eliminate a lot of boilerplate around the
most common uses of `llvm-hs` as a compiler backend. 

*Very much a work in progress.*

TODO

- [ ] Wrap all basic arithmetic, vector expressions.
- [ ] Figure out types for `call` to allow self recursive definitions. 
- [ ] Handle variadic functions?
- [ ] Metadata generation
- [ ] Interface for non-standard calling conventions and linkage

License
-------

MIT
