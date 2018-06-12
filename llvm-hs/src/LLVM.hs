{-|
Module : LLVM
Description: An interface to use LLVM in all capacities.
Copyright: (c) Moritz Kiefer 2018
               Stephen Diehl 2018
               Benjamin Scarlett 2016
Maintainer: moritz.kiefer@purelyfunctional.org
-}
module LLVM (
  module LLVM.Module
  -- * Overview of the @llvm-hs@ library ecosystem
  -- $ecosystem

  -- * Constructing the C++ representation of an LLVM module
  -- $moduleconstruction

  -- * #objectcode# Generating object code
  -- $objectcode

  -- * #jitcompilation# JIT compilation
  -- $jitcompilation
  ) where

import LLVM.Module

import LLVM.AST ()

{- $ecosystem

The main two libraries in the @llvm-hs@ ecosystem are @llvm-hs-pure@
and @llvm-hs@.

* @llvm-hs-pure@ defines a pure Haskell representation of the LLVM
AST. It has no dependency on the LLVM C/C++ libraries so even if you
have trouble installing those or want to avoid that dependency, you
should be able to use it. The documentation
in [LLVM.AST](https://hackage.haskell.org/package/llvm-hs-pure/docs/LLVM-AST.html)
describes the different options for constructing the AST.

* @llvm-hs@ then builds upon @llvm-hs-pure@ and provides the actual FFI
bindings to LLVM’s C++ libraries. Most importantly this includes
bidirectional conversions from the Haskell representation of an LLVM
module to the C++ representation and the other way around.

Once you have constructed the C++ representation, there are two main options:

1. Generate object code as described in "LLVM#objectcode"
2. or JIT compile the module as described in "LLVM#jitcompilation".

In addition to @llvm-hs@ and @llvm-hs-pure@, there are a couple of
other libraries that you be interested in:

* [llvm-hs-pretty](https://hackage.haskell.org/package/llvm-hs-pretty)
  a pure Haskell prettyprinter for the AST in @llvm-hs-pure@. This is
  useful if you just want to render your AST to LLVM’s textual IR
  format either for debugging purposes (@llc@ will usually give pretty
  good error messages for invalid IR) or because you prefer to call
  the LLVM CLI tools over the linking against the LLVM libraries.

* [llvm-hs-typed](https://github.com/llvm-hs/llvm-hs-typed) contains a
  strongly-typed wrapper for the AST in @llvm-hs-pure@ which makes it
  harder to accidentally construct invalid LLVM IR.

* [llvm-hs-quote](https://github.com/llvm-hs/llvm-hs-quote) contains a
  Haskell quasiquoter that can be used for splicing larger chunks of
  existing LLVM IR into your Haskell code.

Finally, there is a [translation](https://github.com/llvm-hs/llvm-hs-kaleidoscope) of
LLVM’s official Kaleidoscope tutorial to @llvm-hs@ and you can find
small, self-contained examples covering various parts of the API in
the [llvm-hs-examples](https://github.com/llvm-hs/llvm-hs-examples)
repository.
-}

{- $moduleconstruction

Interacting with the LLVM libraries requires that you first construct
the C++ representation of an LLVM `Module`.

The most common way of doing that is to first construct the Haskell
representation of an LLVM module using @llvm-hs-pure@. You can then
use `withModuleFromAST` to convert the Haskell AST to the C++
representation.

Alternatively, you can also construct a module from LLVM’s textual IR
or the binary bitcode format using `withModuleFromLLVMAssembly` and
`withModuleFromBitcode`.

-}

{- $objectcode

Once you have constructed the C++ representation of an LLVM `Module`,
you can generate an object file using `moduleObject` which will give
you a `Data.ByteString.ByteString` or write it to a file using
`writeObjectToFile`. To construct the `TargetMachine` for these
functions you can use `LLVM.Target.withHostTargetMachine` if you want
to generate object code for the machine you are currently running on
or use `LLVM.Target.withTargetMachine` and customize the target
machine based on your needs.
-}

{- $jitcompilation

In addition to generating object code, you can also JIT-compile LLVM
modules and call functions in the resulting `Module` from Haskell.

LLVM has several JIT compilers but ORC JIT is the one that is actively
being developed and the one best supported by @llvm-hs@.

To use ORC JIT you first have to create a
`LLVM.OrcJIT.CompileLayer`. You can then use `LLVM.OrcJIT.withModule`
to add an LLVM module to the compile layer and finally use
`LLVM.Internal.OrcJIT.CompileLayer.findSymbol` to get the address of a
symbol in the module.  In most cases, you want to lookup the address
of a function so you have to first convert the `Foreign.Ptr.WordPtr`
to a `Foreign.Ptr.FunPtr` using `Foreign.Ptr.wordPtrToPtr` and
`Foreign.Ptr.castPtrToFunPtr`. Then you can use a foreign dynamic
import to construct a Haskell function which will call the function
located at the `Foreign.Ptr.FunPtr`.
-}
