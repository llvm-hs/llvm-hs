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
