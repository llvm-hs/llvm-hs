## 9.0.1.1 (2021-06-XX)

* Eliminate hard-coded assumption of 32-bit `size_t`
* Handle type resolution through `NamedTypeReference` correctly: type resolution in LLVM depends on module state by design
* Support the LLVM `NoFree` attribute
* Support unbracketed management of `Module`, `Context` with explicit creation and destruction functions
* Add basic support for the OrcJITv2 API
* Various small fixes and improvements to eliminate build warnings

## 9.0.1 (2019-09-28)

* Fix build with Clang on MacOS.
* Expose `LLVMOffsetOfElement` as `getOffsetOfElement`.

## 9.0.0 (2019-09-06)

* Support GHC 8.8
* Update to LLVM 9.0
  * Note that this is the last LLVM release that supports the current
    OrcJIT APIs and `llvm-hs` does not yet have bindings for OrcJIT
    V2. If you are interested in working on that please get in
    touch. See https://github.com/llvm-hs/llvm-hs/issues/276 for more
    details.
  * The `sanitizeAddresses` field from `MachineCodeOptions` has been
    removed following a change in LLVM.
  * The `MemorySanitizer` now has two new options `kernel` and
    `recover` matching the options in LLVM.


## 8.1.0

* Change `withHostTargetMachine` to accept the code model, relocation model,
  and optimization level as arguments. The old behaviour is available
  under the name `withHostTargetMachineDefault`.

## 8.0.0 (2019-03-10)

* Upgrade to LLVM 8
* It is now possible to define multiple variables with an empty name
  without causing a name collision. Variables with an empty name
  cannot be referenced. This fixes an issue caused by using
  IRBuilder’s `extern` helper.

## 7.0.1 (2018-09-29)

* Support llvm-config executables named `llvm-config-7` in addition to
  `llvm-config-7.0` and `llvm-config`.

## 7.0.0 (2018-09-28)

* Throw an `EncodeException` if a local variable is defined multiple
  times. This is consistent with `llc` which also forbids reusing
  variable names.
* Update for LLVM 7.0:
  * The ORC JIT API has been changed to reflect the changes in LLVM
    itself. The test suite should give you a good idea of how to adapt
    your code but here is a summary:
    * There is now a separate `ExecutionSession` that tracks module handles.
    * `SymbolResolver` only has a single resolver function.
    * Creating an `ObjectLinkingLayer` requires a way to get the
      symbol resolver for a module.
    * Creating a `CompileOnDemandLayer` requires a way to set and get
      the symbol resolver for a module.

## 6.3.0 (2018-06-12)

* `findSymbol` and `findSymbolIn` now return `Either JITSymbolError
  JITSymbol` if the JIT symbol has the error flag set or the address
  is 0. (This is consistent with how LLVM treats JIT symbols).
* The return type of `SymbolResolverFn` has been changed to `Either
  JITSymbolError JITSymbol`. It is fine to return a 0 address in
  `Right` so existing resolvers can be adapted by wrapping the result
  in `Right`.
* Fixed a bug where instructions were constant-folded during
  encoding. This caused problems since the API available on a Constant
  is not the same as the one on an Instruction (e.g., we cannot set
  metadata on a Constant).
* Fix use-after-free in `createObjectFile`.
* Add `withObjectFile` wrapper for `createObjectFile` and
  `disposeObjectFile`.
* Add API for looking up symbols in the `LinkingLayer`.
* Add `LLVM.OrcJIT.CompileLayer` and `LLVM.OrcJIT.LinkingLayer`
  modules that reexport the internal modules. The
  `findSymbol/findSymbolIn` methods that have previously been exported
  from `LLVM.OrcJIT` can now be found in `LLVM.OrcJIT.CompileLayer`.

## 6.2.0 (2018-05-08)

* Remove field prefixes from `DIDerivedType`, `DIBasicType` and
  `DISubroutineType` to make the API consistent with the other debug
  metadata types.
* Change the type of the scope fields in `DIModule` and `DINamespace`
  to `Maybe (MDRef DIScope)` to reflect that they can be optional.

## 6.1.1 (2018-05-06)

* Fix the source distribution by adding missing files to extra-source-files.

## 6.1.0 (2018-05-05)

* Remove the `MetadataNodeReference` constructor. References to
  metadata nodes are now encoded using the polymorphic `MDRef` type.
* Add support for encoding and decoding debug metadata. Thanks to
  @xldenis who started that effort!
* Drop support for GHC 7.10.
* Support decoding/encoding of metadata in `GlobalVariable` and `Function`.
* Fix check that the type of `GlobalReference` is correct in the
  presence of automatic renamings due to name collisions.
* Extract LinkingLayer into a separate module.

## 6.0.0 (2018-03-06)

* Support for LLVM 6.0, take a look at the changelog of llvm-hs-pure for details.
* Add `AggregateZero` for zero-initializing structs, arrays and vectors. Previously `Null`
  was used for null pointers as  well as zero-inializing aggregates. The new behavior reflects
  LLVM’s internal representation and the C++-API.
* Enforce that `Null` is only used on pointer types. Existing uses of `Null` on arrays, structs and
  vector must be changed to the newly introduced `AggregateZero`.

## 5.1.3 (2018-01-06)

* Add bindings to `loadLibraryPermamently` and `getSymbolAddressInProcess`.

## 5.1.2 (2017-12-19)

* Reupload of 5.1.1 since [Hackage broke](https://github.com/haskell/hackage-server/issues/643) during the original upload.

## 5.1.1 (2017-12-16)

* Fix argument order in `LLVM_Hs_CreateTargetMachine`. This affects `withTargetMachine` and `withHostTargetMachine`.
* Add support for `MCTargetOptions`.

## 5.1.0 (2017-10-12)

### Bugfixes

* Set target options in `withTargetMachine`. Previously the options
  passed there were simply ignored.
* Fix decoding of constant vectors.
* Fix decoding of function attributes in calls.

### Enhancements

* Support for more target options.
* Suport string attributes as parameter attributes.
* Support more calling conventions.
* Support `NoTail` `TailCallKind`.

## 5.0.0 (2017-09-07)

* Support for LLVM 5.0

    We only give a summary of the changes affecting the public API of `llvm-hs` here.
    Please refer to the official
    [release notes for LLVM 5.0](http://releases.llvm.org/5.0.0/docs/ReleaseNotes.html)
    for an overview of all changes in LLVM 5.0.

    * The `X86_64_Win64` calling convention is now called `Win64`.
    * There is a new `Speculatable` function attribute.
    * The `CrossThread` synchronization scope has been removed. There is
      now a new `System` synchronization scope.
    * The `OrcJIT`-API now operates on individual modules instead of
      sets of modules.
    * The `lessPreciseFloatingPointMultiplyAddOption` field has been
      removed from the target options.
    * The `compressDebugSections` option field is now of type
      `DebugCompressionType` instead of `Bool`.
    * The `BasicBlockVectorize` pass has been removed. You should use
      `SuperwordLevelParallelismVectorize` instead.

* Throw 'EncodeException' when the type supplied in a
  'GlobalReference' does not match the type of the expression.
* Throw 'EncodeException' when the result of instructions returning
  void is named using ':='.

## 4.2.0 (2017-06-20)

* Revamp OrcJIT API
  * The user facing API is now exposed using `LLVM.OrcJIT`.
  * All user facing functions have been documented.
  * In addition the bracket-style API, there are now `new*` and
    `dispose*` functions making it easier to ingegrate OrcJIT in
    custom monad transformer stacks.
  * There is a new `CompileLayer` typeclass which abstracts over the
    various compile layers in `OrcJIT`.
* Support QuickCheck 2.10

## 4.1.0 (2017-05-17)

* Switch most of the API from `String` to `ByteString`.
* Switch from ExceptT to using exceptions.
  See `LLVM.Exception` for an overview of the exceptions potentially thrown.

## 4.0.1

* Fix linking of system libraries

## 4.0.0 (initial release, changes in comparison to llvm-general)

* Move modules from `LLVM.General*` to `LLVM.*`
* Support for LLVM 4.0
* Improved support for LLVM’s exception handling instructions
* `-fshared-llvm` is now supported on windows (thanks to @RyanGLScott)
* Default to `-fshared-llvm`
* Expose `LLVM.Internal.*` modules.
