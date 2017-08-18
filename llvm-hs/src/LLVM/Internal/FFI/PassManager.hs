{-# LANGUAGE
  TemplateHaskell,
  ForeignFunctionInterface,
  CPP
  #-}

module LLVM.Internal.FFI.PassManager where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.Cleanup
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.Target
import LLVM.Internal.FFI.Transforms

import qualified LLVM.Transforms as G

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

foreign import ccall unsafe "LLVMAddAnalysisPasses" addAnalysisPasses ::
  Ptr TargetMachine -> Ptr PassManager -> IO ()

foreign import ccall unsafe "LLVMAddTargetLibraryInfo" addTargetLibraryInfoPass' ::
  Ptr TargetLibraryInfo -> Ptr PassManager -> IO ()

addTargetLibraryInfoPass :: Ptr PassManager -> Ptr TargetLibraryInfo -> IO ()
addTargetLibraryInfoPass = flip addTargetLibraryInfoPass'

$(do
  let declareForeign :: TH.Name -> [TH.Type] -> TH.DecsQ
      declareForeign hName extraParams = do
        let n = TH.nameBase hName
            passTypeMapping :: TH.Type -> TH.TypeQ
            passTypeMapping t = case t of
              TH.ConT h | h == ''Word -> [t| CUInt |]
                        | h == ''G.GCOVVersion -> [t| CString |]
              -- some of the LLVM methods for making passes use "-1" as a special value
              -- handle those here
              TH.AppT (TH.ConT mby) t' | mby == ''Maybe ->
                case t' of
                  TH.ConT h | h == ''Bool -> [t| NothingAsMinusOne Bool |]
                            | h == ''Word -> [t| NothingAsMinusOne Word |]
                            | h == ''FilePath -> [t| NothingAsEmptyString CString |]
                  _ -> typeMapping t
              _ -> typeMapping t
        foreignDecl
          (cName n)
          ("add" ++ n ++ "Pass")
          ([[t| Ptr PassManager |]]
           ++ [[t| Ptr TargetMachine |] | needsTargetMachine n]
           ++ map passTypeMapping extraParams)
          (TH.tupleT 0)
#if __GLASGOW_HASKELL__ < 800
  TH.TyConI (TH.DataD _ _ _ cons _) <- TH.reify ''G.Pass
#else
  TH.TyConI (TH.DataD _ _ _ _ cons _) <- TH.reify ''G.Pass
#endif
  liftM concat $ forM cons $ \con -> case con of
    TH.RecC n l -> declareForeign n [ t | (_,_,t) <- l ]
    TH.NormalC n [] -> declareForeign n []
    _ -> error "pass descriptor constructors with fields need to be records"
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
    Ptr PassManagerBuilder -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableUnrollLoops" passManagerBuilderSetDisableUnrollLoops ::
    Ptr PassManagerBuilder -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderSetDisableSimplifyLibCalls" passManagerBuilderSetDisableSimplifyLibCalls ::
    Ptr PassManagerBuilder -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderUseInlinerWithThreshold" passManagerBuilderUseInlinerWithThreshold ::
    Ptr PassManagerBuilder -> CUInt -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateFunctionPassManager" passManagerBuilderPopulateFunctionPassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateModulePassManager" passManagerBuilderPopulateModulePassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> IO ()

foreign import ccall unsafe "LLVMPassManagerBuilderPopulateLTOPassManager" passManagerBuilderPopulateLTOPassManager ::
    Ptr PassManagerBuilder -> Ptr PassManager -> CUChar -> CUChar -> IO ()

foreign import ccall unsafe "LLVM_Hs_PassManagerBuilderSetLibraryInfo" passManagerBuilderSetLibraryInfo ::
    Ptr PassManagerBuilder -> Ptr TargetLibraryInfo -> IO ()

foreign import ccall unsafe "LLVM_Hs_PassManagerBuilderSetLoopVectorize" passManagerBuilderSetLoopVectorize ::
    Ptr PassManagerBuilder -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVM_Hs_PassManagerBuilderSetSuperwordLevelParallelismVectorize" passManagerBuilderSetSuperwordLevelParallelismVectorize ::
    Ptr PassManagerBuilder -> LLVMBool -> IO ()
