module LLVM.Internal.OrcJITV2
  ( ExecutionSession
  , withExecutionSession
  , lookupSymbol
  , createJITDylib
  , getJITDylibByName
  , addDynamicLibrarySearchGeneratorForCurrentProcess
  , addDynamicLibrarySearchGenerator
  , ThreadSafeContext
  , withThreadSafeContext
  , createThreadSafeContext
  , disposeThreadSafeContext
  , withThreadSafeModule
  , createThreadSafeModule
  , disposeThreadSafeModule
  , ObjectLayer
  , createRTDyldObjectLinkingLayer
  , disposeObjectLayer
  , withRTDyldObjectLinkingLayer
  , IRLayer
  , withIRCompileLayer
  , createIRCompileLayer
  , disposeIRCompileLayer
  , addModule
  , mangleSymbol
  -- , JITEvaluatedSymbol
  ) where

-- FIXME(llvm-12): Clean up this file.

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Foreign.C
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.Module (Module, readModule)
-- import LLVM.Internal.OrcJIT (ExecutionSession(..), JITDylib(..), withExecutionSession, MangledSymbol, JITSymbol, JITSymbolError)
import LLVM.Internal.OrcJIT (ExecutionSession(..), JITDylib(..), withExecutionSession, MangledSymbol)
import LLVM.Internal.Target (TargetMachine(..))

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.OrcJITV2 as FFI
import qualified LLVM.Internal.FFI.Target as FFI

-- newtype JITEvaluatedSymbol = JITEvaluatedSymbol (Ptr FFI.JITEvaluatedSymbol, Word8)

newtype ThreadSafeContext = ThreadSafeContext (Ptr FFI.ThreadSafeContext)

newtype ThreadSafeModule = ThreadSafeModule (Ptr FFI.ThreadSafeModule)

data IRLayer = IRLayer
  { _getIRLayer :: Ptr FFI.IRLayer
  , _getDataLayout :: Ptr FFI.DataLayout
  }
newtype ObjectLayer = ObjectLayer (Ptr FFI.ObjectLayer)

createJITDylib :: ExecutionSession -> String -> IO JITDylib
createJITDylib (ExecutionSession es) s = withCString s
  (fmap JITDylib . FFI.createJITDylib es)

getJITDylibByName :: ExecutionSession -> String -> IO JITDylib
getJITDylibByName (ExecutionSession es) s = withCString s
  (fmap JITDylib . FFI.getJITDylibByName es)

addDynamicLibrarySearchGeneratorForCurrentProcess :: IRLayer -> JITDylib -> IO ()
addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer (JITDylib dylib) =
  FFI.addDynamicLibrarySearchGeneratorForCurrentProcess dylib (_getDataLayout compileLayer)

addDynamicLibrarySearchGenerator :: IRLayer -> JITDylib -> String -> IO ()
addDynamicLibrarySearchGenerator compileLayer (JITDylib dylib) s = withCString s $ \cStr ->
  FFI.addDynamicLibrarySearchGenerator dylib (_getDataLayout compileLayer) cStr

-- | Mangle a symbol according to the data layout stored in the
-- 'CompileLayer'.
mangleSymbol :: IRLayer -> ShortByteString -> IO MangledSymbol
mangleSymbol compileLayer symbol = flip runAnyContT return $ do
  mangledSymbol <- alloca
  symbol' <- encodeM symbol
  anyContToM $ bracket
    (FFI.getMangledSymbol mangledSymbol symbol' (_getDataLayout compileLayer))
    (\_ -> FFI.disposeMangledSymbol =<< peek mangledSymbol)
  decodeM =<< peek mangledSymbol

-- NOTE(llvm-12): This is commented because finding via *MangledSymbol* is not
-- yet supported. Supporting this function seems important and desirable because
-- "looking up mangled symbols" is platform-independent while "looking up
-- symbols directly via mangled string name" is not.
{-
-- | @'findSymbolIn' layer handle symbol exportedSymbolsOnly@ searches for
-- @symbol@ in the context of the module represented by @handle@. If
-- @exportedSymbolsOnly@ is 'True' only exported symbols are searched.
findSymbolIn :: IRLayer -> MangledSymbol -> Bool -> IO (Either JITSymbolError JITSymbol)
findSymbolIn compileLayer symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbolIn compileLayer symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol
-}

-- TODO(llvm-12): Consider removing "looking up symbols directly via mangled
-- string name", which is platform-dependent. See comment above on
-- @findSymbolIn@. Example: platform-dependent @main@ vs @_main@ symbol name.
lookupSymbol :: ExecutionSession -> JITDylib -> String -> IO WordPtr
lookupSymbol (ExecutionSession es) (JITDylib dylib) s = withCString s $ \cStr ->
  FFI.lookupSymbol es dylib cStr

createThreadSafeContext :: IO ThreadSafeContext
createThreadSafeContext = ThreadSafeContext <$> FFI.createThreadSafeContext

disposeThreadSafeContext :: ThreadSafeContext -> IO ()
disposeThreadSafeContext (ThreadSafeContext ctx) = FFI.disposeThreadSafeContext ctx

withThreadSafeContext :: (ThreadSafeContext -> IO a) -> IO a
withThreadSafeContext = bracket createThreadSafeContext disposeThreadSafeContext

createThreadSafeModule :: Module -> IO ThreadSafeModule
createThreadSafeModule m = do
  mPtr <- readModule m
  ThreadSafeModule <$> FFI.createThreadSafeModule mPtr

disposeThreadSafeModule :: ThreadSafeModule -> IO ()
disposeThreadSafeModule (ThreadSafeModule m) = FFI.disposeThreadSafeModule m

withThreadSafeModule :: Module -> (ThreadSafeModule -> IO a) -> IO a
withThreadSafeModule m = bracket (createThreadSafeModule m) disposeThreadSafeModule

createRTDyldObjectLinkingLayer :: ExecutionSession -> IO ObjectLayer
createRTDyldObjectLinkingLayer (ExecutionSession es) =
  ObjectLayer <$> FFI.createRTDyldObjectLinkingLayer es

disposeObjectLayer :: ObjectLayer -> IO ()
disposeObjectLayer (ObjectLayer ol) = FFI.disposeObjectLayer ol

withRTDyldObjectLinkingLayer :: ExecutionSession -> (ObjectLayer -> IO a) -> IO a
withRTDyldObjectLinkingLayer es =
  bracket
    (createRTDyldObjectLinkingLayer es)
    disposeObjectLayer

createIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> IO IRLayer
createIRCompileLayer (ExecutionSession es) (ObjectLayer ol) (TargetMachine tm) = do
  dl <- FFI.createTargetDataLayout tm
  il <- FFI.createIRCompileLayer es ol tm
  pure $ IRLayer il dl

disposeIRCompileLayer :: IRLayer -> IO ()
disposeIRCompileLayer (IRLayer il _) = FFI.disposeIRLayer il

withIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> (IRLayer -> IO a) -> IO a
withIRCompileLayer es ol tm =
  bracket
    (createIRCompileLayer es ol tm)
    disposeIRCompileLayer

addModule :: ThreadSafeModule -> JITDylib -> IRLayer -> IO ()
addModule (ThreadSafeModule m) (JITDylib dylib) (IRLayer il dl) = do
  FFI.irLayerAddModule m dylib dl il
