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
