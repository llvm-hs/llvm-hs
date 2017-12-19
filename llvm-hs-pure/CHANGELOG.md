## 5.1.1 (2017-12-16)

* Add a completely new API for building modules in a monadic style similar to the IRBuilder provided by LLVM’s C++ API. The modules can be found in `LLVM.IRBuilder`. An example can be found in the readme and in the test suite.
* Add an API for getting the type of LLVM values in
  `LLVM.AST.Typed`. This is primarily intended to be used in other
  libraries that build upon `llvm-hs-pure` such as `llvm-hs-pretty`.

## 5.1.0 (2017-10-12)

### Enhancements

* Suport string attributes as parameter attributes
* Support more calling conventions
* Support `NoTail` `TailCallKind`

## 5.0.0 (2017-09-07)

* Support for LLVM 5.0

    We only give a summary of the changes affecting the public API of `llvm-hs-pure` here.
    Please refer to the official
    [release notes for LLVM 5.0](http://releases.llvm.org/5.0.0/docs/ReleaseNotes.html)
    for an overview of all changes in LLVM 5.0.

    * The `X86_64_Win64` calling convention is now called `Win64`.
    * There is a new `Speculatable` function attribute.
    * The `CrossThread` synchronization scope has been removed. There is
      now a new `System` synchronization scope.

## 4.1.0 (2017-05-17)

* Switch AST to `ByteString`/`ShortByteString` reflecting LLVM’s use
  of C-style strings.
* `preferredAlignment` is now a `Word32` instead of `Maybe Word32`. To
  recover the old behavior set it to the same value as abiAlignment.
* `GlobalAlias` now expects the element type of a pointer type instead
  of the pointer type itself. The address space is passed separately
  via the `addrSpace` field. This makes `GlobalAlias` consistent with
  `GlobalVariable`.
* The `FloatingPointType` constructor now takes a `FloatingPointType` argument
  instead of a width and a `FloatingPointFormat` to more closely match the
  LLVM IR language reference.
* The `IsString` instance of `Name` now throws an error on non-ASCII
  strings instead of silently discarding the upper bytes. There is
  also a new `mkName` function with the same behavior for easier
  discoverability. Non-ASCII names need to be encoded using an arbitrary encoding to
  to a `ShortByteString` which can then be used as a `Name`.

## 4.0.0 (initial release, changes in comparison to llvm-general)

* Move modules from `LLVM.General*` to `LLVM.*`
* Support for LLVM 4.0
* Improved support for LLVM’s exception handling instructions
* `-fshared-llvm` is now supported on windows (thanks to @RyanGLScott)
* Default to `-fshared-llvm`
* Expose `LLVM.Internal.*` modules.
