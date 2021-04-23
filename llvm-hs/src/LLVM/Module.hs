-- | A 'Module' holds a C++ LLVM IR module. 'Module's may be converted to or from strings or Haskell ASTs, or
-- added to an 'LLVM.ExecutionEngine.ExecutionEngine' and so JIT compiled to get function pointers.
module LLVM.Module (
    Module,
    File(..),

    withModuleFromAST,
    createModuleFromAST,
    disposeModule,

    moduleContext,
    moduleAST,

    withModuleFromLLVMAssembly,
    moduleLLVMAssembly,
    writeLLVMAssemblyToFile,

    withModuleFromBitcode,
    moduleBitcode,
    writeBitcodeToFile,

    moduleTargetAssembly,
    writeTargetAssemblyToFile,

    moduleObject,
    writeObjectToFile,

    linkModules
  ) where

import LLVM.Internal.Module
