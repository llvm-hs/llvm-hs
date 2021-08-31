# llvm-hs - Haskell bindings for LLVM

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs.svg?branch=llvm-6)](https://travis-ci.org/llvm-hs/llvm-hs) [![Hackage](https://img.shields.io/hackage/v/llvm-hs.svg)](https://hackage.haskell.org/package/llvm-hs)

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

## LLVM API Coverage

In general, anything you might do via the LLVM C API, you can expect to be able
to do with `llvm-hs`. In addition, many things which are not exposed via the
LLVM C API are supported. For example, the LLVM C API [does not provide any
support](https://llvm.org/doxygen/group__LLVMCSupportTypes.html) for working
with `AttributeSet`s and `AttributeList`s, but `llvm-hs` does.

This is made possible because the `llvm-hs` FFI layer incorporates extensions
to the LLVM C API adding missing functionality from the C++ API which is not
yet exposed via the C API by the upstream LLVM project. As the LLVM C API
becomes more complete, we retire our extensions and wrap the newly added
LLVM functions to keep our FFI layer as small as possible.

If you find you need to use some LLVM functionality which is available via the
C++ API but not via the C API or in  `llvm-hs`, please open an issue and
include links to the relevant entities in the LLVM doxygen-generated
documentation.

## Contributing

We love all kinds of contributions so feel free to open issues for missing LLVM
features, report & fix bugs or report API inconveniences.

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
