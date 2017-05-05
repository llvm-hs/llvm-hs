# llvm-hs - Haskell bindings for LLVM

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs.svg?branch=llvm-4)](https://travis-ci.org/llvm-hs/llvm-hs) [![Hackage](https://img.shields.io/hackage/v/llvm-hs.svg)](https://hackage.haskell.org/package/llvm-hs)

This project aims to provide a relatively complete set of bindings for
the LLVM API. If you find that anything is missing please open an
issue! We generally try to stay close to the LLVM C++-API so you can
consult the LLVM documentation and reuse existing resources.

## Contributing

We love all kinds of contributions so feel free to open issues for
missing LLVM features, report & fix bugs or report API
inconveniences.

## Installing LLVM

### Homebrew

Example using Homebrew on macOS:

```bash
$ brew install llvm-hs/homebrew-llvm/llvm-4.0
```

### Debian/Ubuntu

For Debian/Ubuntu based Linux distributions, the LLVM.org website provides
binary distribution packages. Check [http://apt.llvm.org/](apt.llvm.org) for
instructions for adding the correct package database for your OS version, and
then:

```bash
$ apt-get install llvm-4.0-dev
```

### Nix

Nix users can use the following commands to build the library:

```bash
$ nix-shell
$ cabal new-build llvm-hs
```

The Nix shell uses a pinned version of nixpkgs by default.
You can define the `nixpkgs` argument to use a different nixpkgs tree:

```bash
$ nix-shell --arg nixpkgs '<nixpkgs>'
```

### Building from source

Example of building LLVM from source. Detailed build instructions are available
on the LLVM.org website [here](http://llvm.org/docs/CMake.html). [CMake
3.4.3](http://www.cmake.org/cmake/resources/software.html) and a recent C++
compiler are required, at least Clang 3.1, GCC 4.8, or Visual Studio 2015
(Update 3).

  1. Download and unpack the [LLVM-4.0 source code](http://releases.llvm.org/4.0.0/llvm-4.0.0.src.tar.xz).
     We'll refer to the path the source tree was unpacked to as `LLVM_SRC`.

  2. Create a temporary build directory and `cd` to it, for example:
     ```sh
     mkdir /tmp/build
     cd /tmp/build
     ```

  3. Execute the following to configure the build. Here, `INSTALL_PREFIX` is
     where LLVM is to be installed, for example `/usr/local`:
     ```sh
     cmake $LLVM_SRC -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX -DLLVM_BUILD_LLVM_DYLIB=True -DLLVM_LINK_LLVM_DYLIB=True
     ```
     See [options and variables](http://llvm.org/docs/CMake.html#options-and-variables)
     for a list of additional build parameters you can specify.

  4. Build and install:
     ```sh
     cmake --build .
     cmake --build . --target install
     ```

  5. For macOS only, some additional steps are useful to work around issues related
     to [System Integrity Protection](https://en.wikipedia.org/wiki/System_Integrity_Protection):
     ```sh
     cd $INSTALL_PREFIX/lib
     ln -s libLLVM.dylib libLLVM-4.0.dylib
     install_name_tool -id $PWD/libLTO.dylib libLTO.dylib
     install_name_tool -id $PWD/libLLVM.dylib libLLVM.dylib
     install_name_tool -change '@rpath/libLLVM.dylib' $PWD/libLLVM.dylib libLTO.dylib
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

This project is a fork of the venerable `llvm-general` that aims to improve the public release story, and better provide the interfaces needed for any Haskell project looking to leverage LLVM. Contributions are encouraged.
