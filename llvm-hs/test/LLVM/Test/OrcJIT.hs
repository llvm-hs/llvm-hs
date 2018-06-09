{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module LLVM.Test.OrcJIT where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.Ptr

import LLVM.Internal.PassManager
import qualified LLVM.Internal.FFI.PassManager as FFI
import LLVM.Context
import LLVM.Module
import qualified LLVM.Internal.FFI.Module as FFI
import LLVM.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Target

testModule :: ByteString
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

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver s = putStrLn "nullresolver" >> return (Left (JITSymbolError "unknown symbol"))

resolver :: CompileLayer l => MangledSymbol -> l -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
resolver testFunc compileLayer symbol
  | symbol == testFunc = do
      funPtr <- wrapTestFunc myTestFuncImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (Right (JITSymbol addr defaultJITSymbolFlags))
  | otherwise = findSymbol compileLayer symbol True

moduleTransform :: IORef Bool -> Ptr FFI.Module -> IO (Ptr FFI.Module)
moduleTransform passmanagerSuccessful modulePtr = do
  withPassManager defaultCuratedPassSetSpec { optLevel = Just 2 } $ \(PassManager pm) -> do
    success <- toEnum . fromIntegral <$> FFI.runPassManager pm modulePtr
    writeIORef passmanagerSuccessful success
    pure modulePtr

tests :: TestTree
tests =
  testGroup "OrcJit" [
    testCase "eager compilation" $ do
      withTestModule $ \mod ->
        withHostTargetMachine $ \tm ->
          withObjectLinkingLayer $ \linkingLayer ->
            withIRCompileLayer linkingLayer tm $ \compileLayer -> do
              testFunc <- mangleSymbol compileLayer "testFunc"
              withModule
                compileLayer
                mod
                (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
                \moduleHandle -> do
                  mainSymbol <- mangleSymbol compileLayer "main"
                  Right (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                  result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                  result @?= 42
                  Right (JITSymbol mainFn _) <- findSymbolIn compileLayer moduleHandle mainSymbol True
                  result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                  result @?= 42
                  unknownSymbol <- mangleSymbol compileLayer "unknownSymbol"
                  unknownSymbolRes <- findSymbol compileLayer unknownSymbol True
                  unknownSymbolRes @?= Left (JITSymbolError mempty),

    testCase "IRTransformLayer" $ do
      passmanagerSuccessful <- newIORef False
      withTestModule $ \mod ->
        withHostTargetMachine $ \tm ->
          withObjectLinkingLayer $ \linkingLayer ->
            withIRCompileLayer linkingLayer tm $ \compileLayer -> do
              withIRTransformLayer compileLayer tm (moduleTransform passmanagerSuccessful) $ \compileLayer -> do
                testFunc <- mangleSymbol compileLayer "testFunc"
                withModule
                  compileLayer
                  mod
                  (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
                  \moduleHandle -> do
                    mainSymbol <- mangleSymbol compileLayer "main"
                    Right (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                    result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                    result @?= 42
                    readIORef passmanagerSuccessful @? "passmanager failed",

    testCase "lazy compilation" $ do
      withTestModule $ \mod ->
        withHostTargetMachine $ \tm -> do
          triple <- getTargetMachineTriple tm
          withObjectLinkingLayer $ \linkingLayer ->
            withIRCompileLayer linkingLayer tm $ \baseLayer ->
              withIndirectStubsManagerBuilder triple $ \stubsMgr ->
                withJITCompileCallbackManager triple Nothing $ \callbackMgr ->
                  withCompileOnDemandLayer baseLayer tm (\x -> return [x]) callbackMgr stubsMgr False $ \compileLayer -> do
                    testFunc <- mangleSymbol compileLayer "testFunc"
                    withModule
                      compileLayer
                      mod
                      (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
                      \moduleHandle -> do
                        mainSymbol <- mangleSymbol compileLayer "main"
                        Right (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                        result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                        result @?= 42
  ]
