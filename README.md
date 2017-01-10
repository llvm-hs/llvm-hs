# llvm-hs - Haskell bindings for LLVM

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs.svg?branch=llvm-4)](https://travis-ci.org/llvm-hs/llvm-hs)

This project aims to provide a relatively complete set of bindings for
the LLVM API. If you find that anything is missing please open an
issue! We generally try to stay close to the LLVM C++-API so you can
consult the LLVM documentation and reuse existing resources.

## Contributing

We love all kinds of contributions so feel free to open issues for
missing LLVM features, report & fix bugs or report API
inconveniences.

## Building

Example using Homebrew on Mac OS X:

```bash
$ brew update
$ brew install libffi
$ brew install homebrew/versions/llvm39 --all-targets
```

For Debian/Ubuntu based Linux distributions, the LLVM.org website provides
binary distribution packages. Check [apt.llvm.org](apt.llvm.org) for
instructions for adding the correct package database for your OS version, and
then:

```bash
$ apt-get install llvm-3.9-dev
```

### Loading llvm-hs in stack ghci

If you are seeing linker errors when running `stack ghci`, you need to
add the following lines to your `stack.yaml`. The exact options needed
might vary between systems.

```
ghc-options:
  llvm-general: -pgml g++ -optl-Wl,-lLLVM
```

## Versioning

Trying to represent the version of LLVM in the version number but also
allowing for version bumps in the bindings themselves while respecting
the [PVP](http://pvp.haskell.org/) can be tricky. Luckily LLVM is
switching to a
[new versioning scheme](http://blog.llvm.org/2016/12/llvms-new-versioning-scheme.html)
of `major.0.patch` starting from version `4.0`. This means that we can
use the last two components for these bindings while the first
component indicates the version of LLVM. A special case are the
versions `3.major.minor` that represent bindings to LLVM 3.9. Bindings
to earlier versions are not provided.

## How is this related to llvm-general?

This project is a fork of `llvm-general`. `llvm-general` is a great
project but after trying to contribute our ideas with limited success
and the fact that the single maintainer slows down progress has led to
creating a fork.
