{-# LANGUAGE 
  MultiParamTypeClasses
  #-}
module LLVM.Internal.FastMathFlags where

import LLVM.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont
import Control.Monad.State
import Control.Exception

import Data.Bits

import qualified LLVM.Internal.FFI.Builder as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

import LLVM.Internal.Coding
import LLVM.Internal.EncodeAST

import qualified LLVM.AST as A

instance EncodeM IO A.FastMathFlags FFI.FastMathFlags where
  encodeM f = return $ foldr1 (.|.) [
               if a f then b else 0
               | (a,b) <- [
                (A.allowReassoc, FFI.fastMathFlagsAllowReassoc),
                (A.noNaNs, FFI.fastMathFlagsNoNaNs),
                (A.noInfs, FFI.fastMathFlagsNoInfs),
                (A.noSignedZeros, FFI.fastMathFlagsNoSignedZeros),
                (A.allowReciprocal, FFI.fastMathFlagsAllowReciprocal),
                (A.allowContract, FFI.fastMathFlagsAllowContract),
                (A.approxFunc, FFI.fastMathFlagsApproxFunc)
               ] 
              ]

instance EncodeM EncodeAST A.FastMathFlags () where
  encodeM f = do 
    f <- liftIO $ encodeM f
    builder <- gets encodeStateBuilder
    anyContToM $ bracket (FFI.setFastMathFlags builder f) (\() -> FFI.setFastMathFlags builder 0)

instance Monad m => DecodeM m A.FastMathFlags FFI.FastMathFlags where
  decodeM f = return A.FastMathFlags {
                A.allowReassoc = FFI.fastMathFlagsAllowReassoc .&. f /= 0,
                A.noNaNs = FFI.fastMathFlagsNoNaNs .&. f /= 0,
                A.noInfs = FFI.fastMathFlagsNoInfs .&. f /= 0,
                A.noSignedZeros = FFI.fastMathFlagsNoSignedZeros .&. f /= 0,
                A.allowReciprocal = FFI.fastMathFlagsAllowReciprocal .&. f /= 0,
                A.allowContract = FFI.fastMathFlagsAllowContract .&. f /= 0,
                A.approxFunc = FFI.fastMathFlagsApproxFunc .&. f /= 0
              }

