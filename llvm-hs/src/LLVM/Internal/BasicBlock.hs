module LLVM.Internal.BasicBlock where

import LLVM.Prelude

import Control.Monad.Trans
import Foreign.Ptr

import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.BasicBlock as FFI
import qualified LLVM.Internal.FFI.Iterate as FFI

import LLVM.Internal.DecodeAST
import LLVM.Internal.Coding
import LLVM.Internal.Instruction ()

import qualified LLVM.AST.Instruction as A

getBasicBlockTerminator :: Ptr FFI.BasicBlock -> DecodeAST (DecodeAST (A.Named A.Terminator))
getBasicBlockTerminator = decodeM <=< (liftIO . FFI.getBasicBlockTerminator)

getNamedInstructions :: Ptr FFI.BasicBlock -> DecodeAST (DecodeAST [A.Named A.Instruction])
getNamedInstructions b = do
  ffiInstructions <- liftIO $ FFI.getXs (FFI.getFirstInstruction b) FFI.getNextInstruction
  let n = length ffiInstructions
  liftM sequence . forM (take (n-1) ffiInstructions) $ decodeM

  
