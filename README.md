# llvm-hs - Haskell bindings for LLVM

[![Build Status](https://github.com/llvm-hs/llvm-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/llvm-hs/llvm-hs/actions/workflows/ci.yml) [![Hackage](https://img.shields.io/hackage/v/llvm-hs.svg)](https://hackage.haskell.org/package/llvm-hs)

This project aims to provide a relatively complete set of bindings for
the LLVM API. If you find that anything is missing please open an
issue! We generally try to stay close to the LLVM C++-API so you can
consult the LLVM documentation and reuse existing resources.

## Getting started

If you’ve worked with LLVM before, take a look at the examples in the
[llvm-hs-examples repo](https://github.com/llvm-hs/llvm-hs-examples).
If not, you can find a translation of the [official LLVM
tutorial](http://llvm.org/docs/tutorial/#kaleidoscope-implementing-a-language-with-llvm)
at https://github.com/llvm-hs/llvm-hs-kaleidoscope. There is also a
[blog series](https://blog.josephmorag.com/tags/llvm/) on writing a C
compiler with the library. In general, we try to stay very close to
the API and AST provided by LLVM itself, so the [LLVM language
reference](http://llvm.org/docs/LangRef.html) is also very useful.

## LLVM API Interface

`llvm-hs` provides an LLVM binding at (roughly) the same level of abstraction
as the official LLVM C API. Because of this, anything you might do with the
LLVM C API, you should expect to be able to do with `llvm-hs`. In addition,
some things which are not handled in the LLVM C API are supported. For example,
the LLVM C API [does not provide any
support](https://llvm.org/doxygen/group__LLVMCSupportTypes.html) for working
with `AttributeSet` and `AttributeList` types, but `llvm-hs` does.

However, the binding to LLVM is only half the story: a lot of advanced
pure-Haskell functionality is built on top of this basic interface in the
`llvm-hs-pure` module, most notably the monadic
[IRBuilder](https://hackage.haskell.org/package/llvm-hs-pure) and
[ModuleBuilder](https://hackage.haskell.org/package/llvm-hs-pure) interfaces
which greatly simplify the task of generating LLVM code from a higher level
abstract syntax. The
[llvm-hs-examples](https://github.com/llvm-hs/llvm-hs-examples/blob/master/irbuilder/Main.hs)
project contains example usage. These high level interfaces are ideal for
implementing the LLVM backend for your code generation project. A good example
is Google's [Dex](https://github.com/google-research/dex-lang) research
language.

## LLVM API Coverage and Philosophy

The `llvm-hs` FFI layer in `LLVM/Internal/FFI` extends the upstream LLVM C API
*adding missing functionality* which upstream has not yet exposed from the C++
API. We also provide some *improved implementations* of buggy or otherwise
problematic functions in the LLVM C API. As the LLVM C API becomes more
complete, we retire our extensions and directly wrap the newly added C API
functions, ensuring our FFI layer is as small as possible.

If you find you need to use some LLVM functionality which is available via the
C++ API but not via the C API or in  `llvm-hs`, please open an issue and
include links to the relevant entities in the LLVM doxygen-generated
documentation.

In general, if it is possible to implement something in Haskell using the LLVM
C API primitives, that is preferable to implementing things in the FFI layer
and merely exposing them to Haskell as wrapped C or C++ functions.

## Contributing

We love all kinds of contributions so feel free to open issues for
missing LLVM features, report & fix bugs or report API
inconveniences.

## Installing LLVM

This branch tracks LLVM HEAD, meaning that the only way to build it is to
use a fresh checkout of LLVM sources.

### Building from source

Example of building LLVM from source. Detailed build instructions are available
on the LLVM.org website [here](http://llvm.org/docs/CMake.html). [CMake
3.4.3](http://www.cmake.org/cmake/resources/software.html) and a recent C++
compiler are required, at least Clang 3.1, GCC 4.8, or Visual Studio 2015
(Update 3).

  1. Download and unpack the [LLVM source code](https://github.com/llvm/llvm-project):
     ```sh
     git clone https://github.com/llvm/llvm-project -b release/12.x --single-branch
     cd llvm-project
     ```

  2. Create a temporary build directory and `cd` to it, for example:
     ```sh
     mkdir build && cd build
     ```

  3. Execute the following to configure the build. Here, `INSTALL_PREFIX` is
     where LLVM is to be installed, for example `/usr/local`:
     ```sh
     cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX -DLLVM_PARALLEL_LINK_JOBS=1 -DLLVM_BUILD_LLVM_DYLIB=True -DLLVM_LINK_LLVM_DYLIB=True ../llvm
     ```
     See [options and variables](http://llvm.org/docs/CMake.html#options-and-variables)
     for a list of additional build parameters you can specify (we especially recommend the
     `ninja` build system).

  4. Build and install:
     ```sh
     cmake --build .
     cmake --build . --target install
     ```

## Versioning

Trying to represent the version of LLVM in the version number but also
allowing for version bumps in the bindings themselves while respecting
the [PVP](http://pvp.haskell.org/) can be tricky. Luckily LLVM switched to a
[new versioning scheme](http://blog.llvm.org/2016/12/llvms-new-versioning-scheme.html)
of `major.0.patch` starting from version `4.0`. This means that we can
use the last two components for these bindings while the first
component indicates the version of LLVM. A special case are the
versions `3.major.minor` that represent bindings to LLVM 3.9. Bindings
to earlier versions are not provided.

## How is this related to llvm-general?

This project is a fork of the venerable `llvm-general` that aims to improve the
public release story, and better provide the interfaces needed for any Haskell
project looking to leverage LLVM. Contributions are encouraged.
