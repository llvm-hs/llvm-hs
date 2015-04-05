module LLVM.General.Internal.BasicBlock where

import LLVM.General.Prelude

import Control.Monad.Trans
import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.BasicBlock as FFI
import qualified LLVM.General.Internal.FFI.Iterate as FFI

import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Instruction ()

import qualified LLVM.General.AST.Instruction as A

getBasicBlockTerminator :: Ptr FFI.BasicBlock -> DecodeAST (DecodeAST (A.Named A.Terminator))
getBasicBlockTerminator = decodeM <=< (liftIO . FFI.getBasicBlockTerminator)

getNamedInstructions :: Ptr FFI.BasicBlock -> DecodeAST (DecodeAST [A.Named A.Instruction])
getNamedInstructions b = do
  ffiInstructions <- liftIO $ FFI.getXs (FFI.getFirstInstruction b) FFI.getNextInstruction
  let n = length ffiInstructions
  liftM sequence . forM (take (n-1) ffiInstructions) $ decodeM

  
