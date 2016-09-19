{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.General.Test.OrcJIT where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import LLVM.General.Test.Support

import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.Ptr

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.OrcJIT
import LLVM.General.OrcJIT.IRCompileLayer (IRCompileLayer, withIRCompileLayer)
import qualified LLVM.General.OrcJIT.IRCompileLayer as IRCompileLayer
import LLVM.General.OrcJIT.CompileOnDemandLayer (CompileOnDemandLayer, withIndirectStubsManagerBuilder, withJITCompileCallbackManager, withCompileOnDemandLayer)
import qualified LLVM.General.OrcJIT.CompileOnDemandLayer as CODLayer
import LLVM.General.Target

testModule :: String
testModule =
  "; ModuleID = '<string>'\n\
  \source_filename = \"<string>\"\n\
  \\n\
  \declare i32 @testFunc()\n\
  \define i32 @main(i32, i8**) {\n\
  \  %3 = call i32 @testFunc()\n\
  \  ret i32 %3\n\
  \}\n"

withTestModule :: (Module -> IO a) -> IO a
withTestModule f = withContext $ \context -> withModuleFromLLVMAssembly' context testModule f

myTestFuncImpl :: IO Word32
myTestFuncImpl = return 42

foreign import ccall "wrapper"
  wrapTestFunc :: IO Word32 -> IO (FunPtr (IO Word32))

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Word32) -> IO Word32

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver s = putStrLn "nullresolver" >> return (JITSymbol 0 (JITSymbolFlags False False))

resolver :: MangledSymbol -> IRCompileLayer -> MangledSymbol -> IO JITSymbol
resolver testFunc compileLayer symbol
  | symbol == testFunc = do
      funPtr <- wrapTestFunc myTestFuncImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (JITSymbol addr (JITSymbolFlags False True))
  | otherwise = IRCompileLayer.findSymbol compileLayer symbol True

codResolver :: MangledSymbol -> CompileOnDemandLayer -> MangledSymbol -> IO JITSymbol
codResolver testFunc compileLayer symbol
  | symbol == testFunc = do
      funPtr <- wrapTestFunc myTestFuncImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (JITSymbol addr (JITSymbolFlags False True))
  | otherwise = CODLayer.findSymbol compileLayer symbol True

tests :: Test
tests =
  testGroup "OrcJit" [
    testCase "eager compilation" $ do
      withTestModule $ \mod ->
        failInIO $ withHostTargetMachine $ \tm ->
          withObjectLinkingLayer $ \objectLayer ->
            withIRCompileLayer objectLayer tm $ \compileLayer -> do
              testFunc <- IRCompileLayer.mangleSymbol compileLayer "testFunc"
              IRCompileLayer.withModuleSet
                compileLayer
                [mod]
                (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
                \moduleSet -> do
                  mainSymbol <- IRCompileLayer.mangleSymbol compileLayer "main"
                  JITSymbol mainFn _ <- IRCompileLayer.findSymbol compileLayer mainSymbol True
                  result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                  result @?= 42,

    testCase "lazy compilation" $ do
      withTestModule $ \mod ->
        failInIO $ withHostTargetMachine $ \tm -> do
          triple <- getTargetMachineTriple tm
          withObjectLinkingLayer $ \objectLayer ->
            withIRCompileLayer objectLayer tm $ \baseLayer ->
              withIndirectStubsManagerBuilder triple $ \stubsMgr ->
                withJITCompileCallbackManager triple Nothing $ \callbackMgr ->
                  withCompileOnDemandLayer baseLayer (\x -> return [x]) callbackMgr stubsMgr False $ \compileLayer -> do
                    testFunc <- CODLayer.mangleSymbol compileLayer "testFunc"
                    CODLayer.withModuleSet
                      compileLayer
                      [mod]
                      (SymbolResolver (codResolver testFunc compileLayer) nullResolver) $
                      \moduleSet -> do
                        mainSymbol <- CODLayer.mangleSymbol compileLayer "main"
                        JITSymbol mainFn _ <- CODLayer.findSymbol compileLayer mainSymbol True
                        result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                        result @?= 42
  ]
