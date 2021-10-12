{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module LLVM.Test.OrcJIT where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import qualified Data.Map.Strict as Map
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.Ptr
import System.Process (callProcess)
import System.IO.Temp (withSystemTempFile)
import System.IO

import LLVM.Internal.PassManager
import LLVM.Internal.ObjectFile (withObjectFile)
import qualified LLVM.Internal.FFI.PassManager as FFI
import LLVM.Context
import LLVM.Module
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.OrcJITV2 as OrcV2
import LLVM.OrcJIT
import qualified LLVM.Internal.OrcJIT.CompileLayer as CL
import qualified LLVM.Internal.OrcJIT.LinkingLayer as LL
import LLVM.Target
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt

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

test2Module :: ByteString
test2Module =
  "; ModuleID = '<string>'\n\
  \source_filename = \"<string>\"\n\
  \\n\
  \define i32 @main() {\n\
  \  ret i32 42\n\
  \}\n"

withTestModule :: (Module -> IO a) -> IO a
withTestModule f = withContext $ \context -> withModuleFromLLVMAssembly' context testModule f

withTest2Module :: (Module -> IO a) -> IO a
withTest2Module f = withContext $ \context -> withModuleFromLLVMAssembly' context test2Module f

myTestFuncImpl :: IO Word32
myTestFuncImpl = return 42

foreign import ccall "wrapper"
  wrapTestFunc :: IO Word32 -> IO (FunPtr (IO Word32))

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Word32) -> IO Word32

resolver :: CompileLayer l => MangledSymbol -> l -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
resolver testFunc compileLayer symbol = do
  if symbol /= testFunc
    then CL.findSymbol compileLayer symbol True
    else do
      funPtr <- wrapTestFunc myTestFuncImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (Right (JITSymbol addr defaultJITSymbolFlags))

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver s = putStrLn "nullresolver" >> return (Left (JITSymbolError "unknown symbol"))

simpleResolver :: CompileLayer l => l -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
simpleResolver compileLayer symbol = CL.findSymbol compileLayer symbol True

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
      resolvers <- newIORef Map.empty
      withTestModule $ \mod ->
        withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es ->
        withObjectLinkingLayer es (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \linkingLayer ->
        withIRCompileLayer linkingLayer tm $ \compileLayer -> do
          testFunc <- mangleSymbol compileLayer "testFunc"
          withModuleKey es $ \k ->
            withSymbolResolver es (SymbolResolver (resolver testFunc compileLayer)) $ \resolver -> do
              modifyIORef' resolvers (Map.insert k resolver)
              withModule compileLayer k mod $ do
                mainSymbol <- mangleSymbol compileLayer "main"
                Right (JITSymbol mainFn _) <- CL.findSymbol compileLayer mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                result @?= 42
                Right (JITSymbol mainFn _) <- CL.findSymbolIn compileLayer k mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                result @?= 42
                unknownSymbol <- mangleSymbol compileLayer "unknownSymbol"
                unknownSymbolRes <- CL.findSymbol compileLayer unknownSymbol True
                unknownSymbolRes @?= Left (JITSymbolError "undefined symbol"),

    testCase "IRTransformLayer" $ do
      passmanagerSuccessful <- newIORef False
      resolvers <- newIORef Map.empty
      withTestModule $ \mod ->
        withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es ->
        withObjectLinkingLayer es (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \linkingLayer ->
        withIRCompileLayer linkingLayer tm $ \compileLayer ->
        withIRTransformLayer compileLayer tm (moduleTransform passmanagerSuccessful) $ \compileLayer ->
        withModuleKey es $ \k -> do
          testFunc <- mangleSymbol compileLayer "testFunc"
          withSymbolResolver es (SymbolResolver (resolver testFunc compileLayer)) $ \resolver -> do
            modifyIORef' resolvers (Map.insert k resolver)
            withModule compileLayer k mod $ do
              mainSymbol <- mangleSymbol compileLayer "main"
              Right (JITSymbol mainFn _) <- CL.findSymbol compileLayer mainSymbol True
              result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
              result @?= 42
              readIORef passmanagerSuccessful @? "passmanager failed",

    testCase "lazy compilation" $ do
      resolvers <- newIORef Map.empty
      let getResolver k = fmap (Map.! k) (readIORef resolvers)
          setResolver k r = modifyIORef' resolvers (Map.insert k r)
      withTestModule $ \mod ->
        withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm -> do
          triple <- getTargetMachineTriple tm
          withExecutionSession $ \es ->
            withObjectLinkingLayer es getResolver $ \linkingLayer ->
            withIRCompileLayer linkingLayer tm $ \baseLayer ->
            withIndirectStubsManagerBuilder triple $ \stubsMgr ->
            withJITCompileCallbackManager es triple Nothing $ \callbackMgr ->
            withCompileOnDemandLayer es baseLayer tm getResolver setResolver (\x -> return [x]) callbackMgr stubsMgr False $ \compileLayer -> do
              testFunc <- mangleSymbol compileLayer "testFunc"
              withModuleKey es $ \k ->
                withSymbolResolver es (SymbolResolver (resolver testFunc baseLayer)) $ \resolver -> do
                  setResolver k resolver
                  withModule compileLayer k mod $ do
                    mainSymbol <- mangleSymbol compileLayer "main"
                    Right (JITSymbol mainFn _) <- CL.findSymbol compileLayer mainSymbol True
                    result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                    result @?= 42,

    testCase "finding symbols in linking layer" $
      withExecutionSession $ \es ->
      withModuleKey es $ \k ->
      withSymbolResolver es (SymbolResolver nullResolver) $ \resolver ->
      withObjectLinkingLayer es (\_ -> pure resolver) $ \linkingLayer -> do
        let inputPath = "./test/main_return_38.c"
        withSystemTempFile "main.o" $ \outputPath _ -> do
          callProcess "gcc" ["-c", inputPath, "-o", outputPath]
          withObjectFile outputPath $ \objFile -> do
            addObjectFile linkingLayer k objFile
            -- Find main symbol by looking into global linking context
            Right (JITSymbol mainFn _) <- LL.findSymbol linkingLayer "main" True
            result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
            result @?= 38
            -- Find main symbol by specificly using object handle for given object file
            Right (JITSymbol mainFn _) <- LL.findSymbolIn linkingLayer k "main" True
            result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
            result @?= 38,

    testCase "OrcV2" $ do
      withTest2Module $ \mod ->
        withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        OrcV2.withExecutionSession $ \es ->
        OrcV2.withThreadSafeContext $ \ctx ->
        OrcV2.withRTDyldObjectLinkingLayer es $ \ol ->
        OrcV2.withIRCompileLayer es ol tm $ \il -> do
          dl <- getTargetMachineDataLayout tm
          OrcV2.irLayerAdd ctx es il mod
          addr <- OrcV2.esLookup es "main"
          let mainFn = mkMain (castPtrToFunPtr $ wordPtrToPtr $ fromIntegral addr)
          result <- mainFn
          result @?= 42
    ]
