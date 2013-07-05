{-# LANGUAGE
  TemplateHaskell,
  ForeignFunctionInterface
  #-}

module LLVM.General.Internal.FFI.PassManager where

import Language.Haskell.TH

import Control.Monad

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.Cleanup
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.Target
import LLVM.General.Internal.FFI.Transforms

import qualified LLVM.General.Transforms as G

data PassManager

foreign import ccall unsafe "LLVMCreatePassManager" createPassManager ::
    IO (Ptr PassManager)

foreign import ccall unsafe "LLVMDisposePassManager" disposePassManager ::
    Ptr PassManager -> IO ()

foreign import ccall unsafe "LLVMRunPassManager" runPassManager ::
    Ptr PassManager -> Ptr Module -> IO CUInt

foreign import ccall unsafe "LLVMCreateFunctionPassManagerForModule" createFunctionPassManagerForModule ::
    Ptr Module -> IO (Ptr PassManager)

foreign import ccall unsafe "LLVMInitializeFunctionPassManager" initializeFunctionPassManager ::
    Ptr PassManager -> IO CUInt

foreign import ccall unsafe "LLVMRunFunctionPassManager" runFunctionPassManager ::
    Ptr PassManager -> Ptr Value -> IO CUInt

foreign import ccall unsafe "LLVMFinalizeFunctionPassManager" finalizeFunctionPassManager ::
    Ptr PassManager -> IO CUInt

$(do
  let declareForeign :: Name -> [Type] -> DecsQ
      declareForeign hName extraParams = do
        let n = nameBase hName
        foreignDecl 
          (cName n)
          ("add" ++ n ++ "Pass")
          ([[t| Ptr PassManager |]] 
           ++ [[t| Ptr TargetMachine |] | needsTargetMachine n]
           ++ map typeMapping extraParams)
          (tupleT 0)

  TyConI (DataD _ _ _ cons _) <- reify ''G.Pass
  liftM concat $ forM cons $ \con -> case con of
    RecC n l -> declareForeign n [ t | (_,_,t) <- l ]
    NormalC n [] -> declareForeign n []
    NormalC n _ -> error "pass descriptor constructors with fields need to be records"
 )

data PassManagerBuilder

foreign import ccall unsafe "LLVMPassManagerBuilderCreate" passManagerBuilderCreate ::
    IO (Ptr PassManagerBuilder) 

foreign import ccall unsafe "LLVMPassManagerBuilderDispose" passManagerBuilderDispose ::
    Ptr PassManagerBuilder -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetOptLevel" passManagerBuilderSetOptLevel ::
    Ptr PassManagerBuilder -> CUInt -> IO () 

foreign import ccall unsafe "LLVMPassManagerBuilderSetSizeLevel" passManagerBuilderSetSizeLevel ::
    Ptr PassManagerBuilder -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableUnitAtATime" passManagerBuilderSetDisableUnitAtATime ::
    Ptr PassManagerBuilder -> CUInt -> IO () 

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableUnrollLoops" passManagerBuilderSetDisableUnrollLoops ::
    Ptr PassManagerBuilder -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableSimplifyLibCalls" passManagerBuilderSetDisableSimplifyLibCalls ::
    Ptr PassManagerBuilder -> CUInt -> IO () 

foreign import ccall unsafe "LLVMPassManagerBuilderUseInlinerWithThreshold" passManagerBuilderUseInlinerWithThreshold ::
    Ptr PassManagerBuilder -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateFunctionPassManager" passManagerBuilderPopulateFunctionPassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> IO () 

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateModulePassManager" passManagerBuilderPopulateModulePassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateLTOPassManager" passManagerBuilderPopulateLTOPassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> CUChar -> CUChar -> IO () 
