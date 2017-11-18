IRBuilder
=========

[![Build Status](https://travis-ci.org/llvm-hs/llvm-irbuilder.svg?branch=master)](https://travis-ci.org/llvm-hs/llvm-irbuilder)

A IRBuilder, starting out as a thin reinterpretation of the C++ IRBuilder inside
of a Haskell State monad. Goal is to eliminate a lot of boilerplate around the
most common uses of `llvm-hs` as a compiler backend. 

TODO

- [x] Wrap all basic arithmetic, vector expressions.
- [x] Figure out types for `call` to allow self recursive definitions. 
- [ ] Handle variadic functions?
- [ ] Metadata generation
- [ ] Rewrite Kaleidoscope tutorial to use new builder monad instead of deriving it from scratch.
- [ ] Integrate with Joachim's typed-IR AST.

License
-------

MIT
