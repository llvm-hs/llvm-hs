## unreleased

* Switch AST to `ByteString`/`ShortByteString` reflecting LLVM’s use
  of C-style strings
* 'preferredAlignment' is now a 'Word32' instead of 'Maybe Word32'. To
  recover the old behavior set it to the same value as abiAlignment.
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
