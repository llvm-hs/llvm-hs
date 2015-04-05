{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions for handling the LLVMContext class. In all other LLVM interfaces,
-- | prefer the newer explicitly thread-aware variants which use contexts
-- | over corresponding older variants which implicitly reference a global context.
-- | This choice allows multiple threads to do independent work with LLVM safely.
module LLVM.General.Internal.FFI.Context where

import LLVM.General.Prelude

import Foreign.Ptr

-- | a blind type to correspond to LLVMContext
data Context

-- | <http://llvm.org/doxygen/group__LLVMCCoreContext.html#gaac4f39a2d0b9735e64ac7681ab543b4c>
foreign import ccall unsafe "LLVMContextCreate" contextCreate ::
    IO (Ptr Context)

-- | <http://llvm.org/doxygen/group__LLVMCCoreContext.html#ga0055cde9a9b2497b332d639d8844a810>
foreign import ccall unsafe "LLVMGetGlobalContext" getGlobalContext ::
    IO (Ptr Context)

-- | <http://llvm.org/doxygen/group__LLVMCCoreContext.html#ga9cf8b0fb4a546d4cdb6f64b8055f5f57>
foreign import ccall unsafe "LLVMContextDispose" contextDispose ::
    Ptr Context -> IO ()
