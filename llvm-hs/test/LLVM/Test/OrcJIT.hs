{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module LLVM.Test.OrcJIT where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import qualified Data.Map.Strict as Map
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Int
import Data.IORef
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.Process (callProcess)
import System.IO.Temp (withSystemTempFile)
import System.Directory
import System.IO

import LLVM.Internal.ObjectFile (withObjectFile)
import LLVM.Passes
import LLVM.Context
import LLVM.Module
import qualified LLVM.Internal.FFI.Module as FFI
import LLVM.OrcJIT
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

test3Module :: ByteString
test3Module =
  "; ModuleID = '<string>'\n\
  \source_filename = \"<string>\"\n\
  \@data = external global i32\n\
  \\n\
  \define i32 @main() {\n\
  \  %1 = load i32, i32* @data\n\
  \  ret i32 %1\n\
  \}\n"

withTestModule :: ByteString -> (Module -> IO a) -> IO a
withTestModule txt f = withContext $ \context -> withModuleFromLLVMAssembly' context txt f

myTestFuncImpl :: IO Word32
myTestFuncImpl = return 42

foreign import ccall "wrapper"
  wrapTestFunc :: IO Word32 -> IO (FunPtr (IO Word32))

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Word32) -> IO Word32

tests :: TestTree
tests =
  testGroup "OrcJIT" [
    testCase "basic self-contained function" $ do
      withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es -> do
          ol <- createRTDyldObjectLinkingLayer es
          il <- createIRCompileLayer es ol tm
          dylib <- createJITDylib es "testDylib"
          withTestModule test2Module $ \m ->
            withClonedThreadSafeModule m $ \tsm -> do
              addModule tsm dylib il
              Right (JITSymbol addr _) <- lookupSymbol es il dylib "main"
              let mainFn = mkMain (castPtrToFunPtr $ wordPtrToPtr $ fromIntegral addr)
              result <- mainFn
              result @?= 42,

    testCase "using symbols in external shared libraries" $
      withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es -> do
          ol <- createRTDyldObjectLinkingLayer es
          il <- createIRCompileLayer es ol tm
          dylib <- createJITDylib es "testDylib"
          let inputPath = "./test/main_return_38.c"
          withSystemTempFile "main.o" $ \outputPath _ -> do
            findExecutable "gcc" >>= \gccPath ->
              case gccPath of
                Nothing -> return ()  -- Tasty.HUnit doesn't seem to support skips?
                Just _  -> do
                  callProcess "gcc" ["-shared", "-fPIC", inputPath, "-o", outputPath]
                  addDynamicLibrarySearchGenerator il dylib outputPath
                  Right (JITSymbol mainFn _) <- lookupSymbol es il dylib "main"
                  result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                  result @?= 38,

    testCase "run optimization passes on a JIT module" $ do
      withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es -> do
          ol <- createRTDyldObjectLinkingLayer es
          il <- createIRCompileLayer es ol tm
          dylib <- createJITDylib es "testDylib"
          withTestModule test2Module $ \m -> do
            runPasses (PassSetSpec [CuratedPassSet 2] Nothing) m
            withClonedThreadSafeModule m $ \tsm -> do
              addModule tsm dylib il
              Right (JITSymbol mainFn _) <- lookupSymbol es il dylib "main"
              result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
              result @?= 42

    testCase "defining absolute symbols" $ do
      withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
        withExecutionSession $ \es -> do
          ol <- createRTDyldObjectLinkingLayer es
          il <- createIRCompileLayer es ol tm
          alloca $ \ptr -> do
            poke ptr (1234 :: Int32)
            dylib <- createJITDylib es "testDylib"
            withMangledSymbol il "data" $ \dataSym -> do
              let flags = defaultJITSymbolFlags { jitSymbolAbsolute = True }
              defineAbsoluteSymbols dylib [(dataSym, JITSymbol (ptrToWordPtr ptr) flags)]
              withTestModule test3Module $ \m -> do
                withClonedThreadSafeModule m $ \tsm -> do
                  addModule tsm dylib il
                  Right (JITSymbol addr _) <- lookupSymbol es il dylib "main"
                  let mainFn = mkMain (castPtrToFunPtr $ wordPtrToPtr $ fromIntegral addr)
                  result <- mainFn
                  result @?= 1234

    -- TODO: Make it possible to use Haskell functions as definition generators
    --       and update to OrcJITv2
    {-
     testCase "eager compilation" $ do
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
                 unknownSymbolRes @?= Left (JITSymbolError mempty),
    -}

    -- TODO: Add IRTransformLayer and translate to OrcJITv2
    {-
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
    -}

    -- TODO: Add support for loading object files and update to OrcJITv2
    {-
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
    -}
    ]
