{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Internal.OrcJIT where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.Bits
import Data.IORef
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.Module (Module, readModule)
import LLVM.Internal.Target (TargetMachine(..))

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.ShortByteString as SBS
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.Target as FFI

--------------------------------------------------------------------------------
-- ExecutionSession
--------------------------------------------------------------------------------

data ExecutionSession = ExecutionSession {
    sessionPtr :: !(Ptr FFI.ExecutionSession),
    sessionCleanups :: !(IORef [IO ()])
  }

-- | Create a new `ExecutionSession`.
createExecutionSession :: IO ExecutionSession
createExecutionSession = ExecutionSession <$> FFI.createExecutionSession <*> newIORef []

-- | Dispose of an `ExecutionSession`. This should be called when the
-- `ExecutionSession` is not needed anymore.
disposeExecutionSession :: ExecutionSession -> IO ()
disposeExecutionSession (ExecutionSession es cleanups) = do
  FFI.endSession es
  sequence_ =<< readIORef cleanups
  FFI.disposeExecutionSession es

-- | `bracket`-style wrapper around `createExecutionSession` and
-- `disposeExecutionSession`.
withExecutionSession :: (ExecutionSession -> IO a) -> IO a
withExecutionSession = bracket createExecutionSession disposeExecutionSession

--------------------------------------------------------------------------------
-- JITSymbol
--------------------------------------------------------------------------------

-- | Contrary to the C++ interface, we do not store the HasError flag
-- here. Instead decoding a JITSymbol produces a sumtype based on
-- whether that flag is set or not.
data JITSymbolFlags =
  JITSymbolFlags {
    jitSymbolWeak :: !Bool -- ^ Is this a weak symbol?
  , jitSymbolCommon :: !Bool -- ^ Is this a common symbol?
  , jitSymbolAbsolute :: !Bool
    -- ^ Is this an absolute symbol? This will cause LLVM to use
    -- absolute relocations for the symbol even in position
    -- independent code.
  , jitSymbolExported :: !Bool -- ^ Is this symbol exported?
  , jitSymbolCallable :: !Bool
  , jitSymbolMaterializationSideEffectsOnly :: !Bool
  }
  deriving (Show, Eq, Ord)

defaultJITSymbolFlags :: JITSymbolFlags
defaultJITSymbolFlags = JITSymbolFlags False False False False False False

data JITSymbol =
  JITSymbol {
    jitSymbolAddress :: !WordPtr, -- ^ The address of the symbol. If
                                  -- you’ve looked up a function, you
                                  -- need to cast this to a 'FunPtr'.
    jitSymbolFlags :: !JITSymbolFlags -- ^ The flags of this symbol.
  }
  deriving (Show, Eq, Ord)

data JITSymbolError = JITSymbolError ShortByteString
  deriving (Show, Eq)

instance Monad m => EncodeM m JITSymbolFlags FFI.JITSymbolFlags where
  encodeM f = return $ foldr1 (.|.) [
      if a f
         then b
         else 0
    | (a,b) <- [
          (jitSymbolWeak, FFI.jitSymbolFlagsWeak),
          (jitSymbolCommon, FFI.jitSymbolFlagsCommon),
          (jitSymbolAbsolute, FFI.jitSymbolFlagsAbsolute),
          (jitSymbolExported, FFI.jitSymbolFlagsExported),
          (jitSymbolCallable, FFI.jitSymbolFlagsCallable),
          (jitSymbolMaterializationSideEffectsOnly, FFI.jitSymbolFlagsMaterializationSideEffectsOnly)
        ]
    ]

instance Monad m => DecodeM m JITSymbolFlags FFI.JITSymbolFlags where
  decodeM f =
    return $ JITSymbolFlags {
      jitSymbolWeak = FFI.jitSymbolFlagsWeak .&. f /= 0,
      jitSymbolCommon = FFI.jitSymbolFlagsCommon .&. f /= 0,
      jitSymbolAbsolute = FFI.jitSymbolFlagsAbsolute .&. f /= 0,
      jitSymbolExported = FFI.jitSymbolFlagsExported .&. f /= 0,
      jitSymbolCallable = FFI.jitSymbolFlagsCallable .&. f /= 0,
      jitSymbolMaterializationSideEffectsOnly = FFI.jitSymbolFlagsMaterializationSideEffectsOnly .&. f /= 0
    }

instance Monad m => EncodeM m MangledSymbol (Ptr FFI.SymbolStringPtr) where
  encodeM (MangledSymbol p) = return p

instance (Monad m, MonadIO m, MonadAnyCont IO m) => EncodeM m JITSymbol (Ptr FFI.JITEvaluatedSymbol) where
  encodeM (JITSymbol addr flags) = do
    flags' <- encodeM flags
    anyContToM $ bracket
      (FFI.createJITEvaluatedSymbol (FFI.TargetAddress $ fromIntegral addr) flags')
      FFI.disposeJITEvaluatedSymbol

instance (MonadIO m, MonadAnyCont IO m) =>
         DecodeM m (Either JITSymbolError JITSymbol) (Ptr FFI.ExpectedJITEvaluatedSymbol) where
  decodeM expectedSym = do
    errMsg <- alloca
    FFI.TargetAddress addr <- liftIO $ FFI.getExpectedSymbolAddress expectedSym errMsg
    rawFlags <- liftIO (FFI.getExpectedSymbolFlags expectedSym)
    if addr == 0 || (rawFlags .&. FFI.jitSymbolFlagsHasError /= 0)
      then do
        errMsg <- decodeM errMsg
        pure (Left (JITSymbolError errMsg))
      else do
        flags <- decodeM rawFlags
        pure (Right (JITSymbol (fromIntegral addr) flags))

--------------------------------------------------------------------------------
-- JITDylib
--------------------------------------------------------------------------------

newtype JITDylib = JITDylib (Ptr FFI.JITDylib)

-- | Create a new 'JITDylib' with the given name.
createJITDylib :: ExecutionSession -> ShortByteString -> IO JITDylib
createJITDylib (ExecutionSession es _) name =
  SBS.useAsCString name $ fmap JITDylib . FFI.createJITDylib es

-- NB: JITDylib unloading is WIP (at least according to some old-looking docs)

-- | Adds a 'JITDylib' definition generator that looks up missing symbols in
-- the namespace of the current process.
addDynamicLibrarySearchGeneratorForCurrentProcess :: IRLayer l => l -> JITDylib -> IO ()
addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer (JITDylib dylib) =
  FFI.addDynamicLibrarySearchGeneratorForCurrentProcess dylib (getDataLayout compileLayer)

-- | Adds a 'JITDylib' definition generator that looks up missing symbols in
-- the namespace of a shared library located at the specified 'FilePath'.
addDynamicLibrarySearchGenerator :: IRLayer l => l -> JITDylib -> FilePath -> IO ()
addDynamicLibrarySearchGenerator compileLayer (JITDylib dylib) s = withCString s $ \cStr ->
  FFI.addDynamicLibrarySearchGenerator dylib (getDataLayout compileLayer) cStr

defineAbsoluteSymbols :: JITDylib -> [(MangledSymbol, JITSymbol)] -> IO ()
defineAbsoluteSymbols (JITDylib dylib) symList =
  runAnyContT' return $ do
    (nsyms, symStrPtrPtr) :: (CUInt, Ptr (Ptr FFI.SymbolStringPtr)) <- encodeM symNames
    (_, symValPtrPtr) :: (CUInt, Ptr (Ptr FFI.JITEvaluatedSymbol)) <- encodeM symVals
    liftIO $ FFI.defineAbsoluteSymbols dylib nsyms symStrPtrPtr symValPtrPtr
  where (symNames, symVals) = unzip symList

-- | Looks up an (unmangled) symbol name in the given 'JITDylib'.
--
-- The symbol is expected to have been added to the 'JITDylib' by the same 'IRLayer'
-- as specified in this function. Using a different 'IRLayer' can cause the lookup
-- to fail due to differences in mangling schemes.
lookupSymbol :: IRLayer l => ExecutionSession -> l -> JITDylib -> ShortByteString -> IO (Either JITSymbolError JITSymbol)
lookupSymbol (ExecutionSession es _) irl (JITDylib dylib) name = SBS.useAsCString name $ \nameStr ->
  runAnyContT' return $ do
    symbol <- anyContToM $ bracket
      (FFI.lookupSymbol es dylib (getMangler irl) nameStr) FFI.disposeExpectedJITEvaluatedSymbol
    decodeM symbol

--------------------------------------------------------------------------------
-- ThreadSafeContext
--------------------------------------------------------------------------------

newtype ThreadSafeContext = ThreadSafeContext (Ptr FFI.ThreadSafeContext)

-- | Create a 'ThreadSafeContext'
createThreadSafeContext :: IO ThreadSafeContext
createThreadSafeContext = ThreadSafeContext <$> FFI.createThreadSafeContext

-- | Dispose of a 'ThreadSafeContext'
disposeThreadSafeContext :: ThreadSafeContext -> IO ()
disposeThreadSafeContext (ThreadSafeContext ctx) = FFI.disposeThreadSafeContext ctx

-- | 'bracket'-style wrapper around 'createThreadSafeContext'
-- and 'disposeThreadSafeContext'.
withThreadSafeContext :: (ThreadSafeContext -> IO a) -> IO a
withThreadSafeContext = bracket createThreadSafeContext disposeThreadSafeContext

--------------------------------------------------------------------------------
-- ThreadSafeModule
--------------------------------------------------------------------------------

newtype ThreadSafeModule = ThreadSafeModule (Ptr FFI.ThreadSafeModule)

-- | Create a 'ThreadSafeModule' with the same content as the input 'Module'.
--
-- The module will get cloned into a fresh LLVM context. The lifetime of the
-- new context is bound to the lifetime of the returned 'ThreadSafeModule'.
cloneAsThreadSafeModule :: Module -> IO ThreadSafeModule
cloneAsThreadSafeModule m = do
  mPtr <- readModule m
  ThreadSafeModule <$> FFI.cloneAsThreadSafeModule mPtr

-- | Dispose of a 'ThreadSafeModule'.
disposeThreadSafeModule :: ThreadSafeModule -> IO ()
disposeThreadSafeModule (ThreadSafeModule m) = FFI.disposeThreadSafeModule m

-- | 'bracket'-style wrapper around 'cloneAsThreadSafeModule'
-- and 'disposeThreadSafeModule'.
withClonedThreadSafeModule :: Module -> (ThreadSafeModule -> IO a) -> IO a
withClonedThreadSafeModule m = bracket (cloneAsThreadSafeModule m) disposeThreadSafeModule

--------------------------------------------------------------------------------
-- ObjectLayer + RTDyldObjectLinkingLayer
--------------------------------------------------------------------------------

-- | A type class implemented by the different OrcJIT object layers.
--
-- See e.g. 'RTDyldObjectLinkingLayer'.
class ObjectLayer l where
  getObjectLayer :: l -> Ptr FFI.ObjectLayer


data RTDyldObjectLinkingLayer = RTDyldObjectLinkingLayer !(Ptr FFI.ObjectLayer)

instance ObjectLayer RTDyldObjectLinkingLayer where
  getObjectLayer (RTDyldObjectLinkingLayer ol) = ol

-- | Create a new 'RTDyldObjectLinkingLayer'.
--
-- The layer will get automatically disposed along with its ExecutionSession.
createRTDyldObjectLinkingLayer :: ExecutionSession -> IO RTDyldObjectLinkingLayer
createRTDyldObjectLinkingLayer (ExecutionSession es cleanups) = do
  ol <- FFI.createRTDyldObjectLinkingLayer es
  modifyIORef' cleanups (FFI.disposeObjectLayer ol :)
  return $ RTDyldObjectLinkingLayer ol

data ObjectLinkingLayer = ObjectLinkingLayer !(Ptr FFI.ObjectLayer)

instance ObjectLayer ObjectLinkingLayer where
  getObjectLayer (ObjectLinkingLayer ol) = ol

createObjectLinkingLayer :: ExecutionSession -> IO ObjectLinkingLayer
createObjectLinkingLayer (ExecutionSession es cleanups) = do
  ol <- FFI.createObjectLinkingLayer es
  modifyIORef' cleanups (FFI.disposeObjectLayer ol :)
  return $ ObjectLinkingLayer ol

addObjectFile :: ObjectLayer l => l -> JITDylib -> FilePath -> IO ()
addObjectFile ol (JITDylib dylib) path = do
  withCString path $ \cStr ->
    FFI.objectLayerAddObjectFile (getObjectLayer ol) dylib cStr

--------------------------------------------------------------------------------
-- IRLayer + IRCompileLayer
--------------------------------------------------------------------------------

-- | A mangled symbol name. Valid only for as long as the IRLayer that created it.
newtype MangledSymbol = MangledSymbol (Ptr FFI.SymbolStringPtr)

-- | A type class implemented by the different OrcJIT IR layers.
--
-- See e.g. 'IRCompileLayer'.
class IRLayer l where
  getIRLayer :: l -> Ptr FFI.IRLayer
  getDataLayout :: l -> Ptr FFI.DataLayout
  getMangler :: l -> Ptr FFI.MangleAndInterner

-- | Add a 'Module' to the specified 'JITDylib'.
--
-- The specified 'IRLayer' will be responsible for compiling the symbols
-- present in the module. The module itself is consumed and __should not be used again__.
addModule :: IRLayer l => ThreadSafeModule -> JITDylib -> l -> IO ()
addModule (ThreadSafeModule m) (JITDylib dylib) irl =
  FFI.irLayerAddModule m dylib (getDataLayout irl) (getIRLayer irl)

mangleSymbol :: IRLayer l => l -> ShortByteString -> IO MangledSymbol
mangleSymbol irl name = SBS.useAsCString name $ \namePtr ->
  MangledSymbol <$> FFI.mangleSymbol (getMangler irl) namePtr

disposeMangledSymbol :: MangledSymbol -> IO ()
disposeMangledSymbol (MangledSymbol symbol) = FFI.disposeMangledSymbol symbol

withMangledSymbol :: IRLayer l => l -> ShortByteString -> (MangledSymbol -> IO a) -> IO a
withMangledSymbol irl name = bracket (mangleSymbol irl name) disposeMangledSymbol

-- | An IR layer that compiles the symbols in a module eagerly.
data IRCompileLayer = IRCompileLayer !(Ptr FFI.IRLayer) !(Ptr FFI.DataLayout) !(Ptr FFI.MangleAndInterner)

instance IRLayer IRCompileLayer where
  getIRLayer    (IRCompileLayer cl _ _) = cl
  getDataLayout (IRCompileLayer _ dl _) = dl
  getMangler    (IRCompileLayer _ _ mg) = mg

-- | Create a new 'IRCompileLayer'.
--
-- The layer will get automatically disposed along with its ExecutionSession.
createIRCompileLayer :: ObjectLayer l => ExecutionSession -> l -> TargetMachine -> IO IRCompileLayer
createIRCompileLayer (ExecutionSession es cleanups) ol (TargetMachine tm) = do
  dl <- FFI.createTargetDataLayout tm
  modifyIORef' cleanups (FFI.disposeDataLayout dl :)
  mg <- FFI.createMangleAndInterner es dl
  modifyIORef' cleanups (FFI.disposeMangleAndInterner mg :)
  cl <- FFI.createIRCompileLayer es (getObjectLayer ol) tm
  modifyIORef' cleanups (FFI.disposeIRLayer cl :)
  return $ IRCompileLayer cl dl mg
