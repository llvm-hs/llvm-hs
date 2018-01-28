## 6.0.0 (unreleased)

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
