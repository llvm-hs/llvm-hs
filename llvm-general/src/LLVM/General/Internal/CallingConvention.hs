{-# LANGUAGE
  MultiParamTypeClasses,
  TemplateHaskell,
  QuasiQuotes
  #-}
module LLVM.General.Internal.CallingConvention where

import LLVM.General.Prelude

import LLVM.General.Internal.Coding
import Foreign.C.Types (CUInt(..))

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import LLVM.General.Internal.FFI.LLVMCTypes (callConvP)

import qualified LLVM.General.AST.CallingConvention as A.CC

instance Monad m => EncodeM m A.CC.CallingConvention FFI.CallConv where
  encodeM cc = return $ 
        case cc of
          A.CC.C -> FFI.callConvC
          A.CC.Fast -> FFI.callConvFast
          A.CC.Cold -> FFI.callConvCold
          A.CC.GHC ->  FFI.CallConv 10
          A.CC.Numbered cc' -> FFI.CallConv (fromIntegral cc')

instance Monad m => DecodeM m A.CC.CallingConvention FFI.CallConv where
  decodeM cc = return $ case cc of
    [callConvP|C|] -> A.CC.C
    [callConvP|Fast|] -> A.CC.Fast
    [callConvP|Cold|] -> A.CC.Cold
    FFI.CallConv (CUInt 10) -> A.CC.GHC
    FFI.CallConv (CUInt ci) | ci >= 64 -> A.CC.Numbered (fromIntegral ci)
