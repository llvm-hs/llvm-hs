{-# LANGUAGE 
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.FastMathFlags where

import LLVM.General.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont
import Control.Monad.State
import Control.Exception

import Data.Bits

import qualified LLVM.General.Internal.FFI.Builder as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.EncodeAST

import qualified LLVM.General.AST as A

instance EncodeM IO A.FastMathFlags FFI.FastMathFlags where
  encodeM A.NoFastMathFlags = return 0
  encodeM A.UnsafeAlgebra = return FFI.fastMathFlagsUnsafeAlgebra
  encodeM f = return $ foldr1 (.|.) [ 
               if a f then b else 0
               | (a,b) <- [
                (A.noNaNs, FFI.fastMathFlagsNoNaNs),
                (A.noInfs, FFI.fastMathFlagsNoInfs),
                (A.noSignedZeros, FFI.fastMathFlagsNoSignedZeros),
                (A.allowReciprocal, FFI.fastMathFlagsAllowReciprocal)
               ] 
              ]

instance EncodeM EncodeAST A.FastMathFlags () where
  encodeM f = do
    f <- liftIO $ encodeM f
    builder <- gets encodeStateBuilder
    anyContToM $ bracket (FFI.setFastMathFlags builder f) (\() -> FFI.setFastMathFlags builder 0)  

instance Monad m => DecodeM m A.FastMathFlags FFI.FastMathFlags where
  decodeM 0 = return A.NoFastMathFlags
  decodeM f | FFI.fastMathFlagsUnsafeAlgebra .&. f /= 0 = return A.UnsafeAlgebra
  decodeM f = return A.FastMathFlags {
                A.noNaNs = FFI.fastMathFlagsNoNaNs .&. f /= 0,
                A.noInfs = FFI.fastMathFlagsNoInfs .&. f /= 0,
                A.noSignedZeros = FFI.fastMathFlagsNoSignedZeros .&. f /= 0,
                A.allowReciprocal = FFI.fastMathFlagsAllowReciprocal .&. f /= 0
              }

