{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.Internal.OrcJIT where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (packCString, useAsCString)
import Data.IORef
import Foreign.C.String
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.Target
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.Target as FFI

-- | A mangled symbol which can be used in 'findSymbol'. This can be
-- created using 'mangleSymbol'.
newtype MangledSymbol = MangledSymbol ByteString
  deriving (Show, Eq, Ord)

instance EncodeM (AnyContT IO) MangledSymbol CString where
  encodeM (MangledSymbol bs) = anyContToM $ useAsCString bs

instance MonadIO m => DecodeM m MangledSymbol CString where
  decodeM str = liftIO $ MangledSymbol <$> packCString str

newtype ExecutionSession = ExecutionSession (Ptr FFI.ExecutionSession)

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
  }
  deriving (Show, Eq, Ord)

defaultJITSymbolFlags :: JITSymbolFlags
defaultJITSymbolFlags = JITSymbolFlags False False False False

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

-- | Specifies how external symbols in a module added to a
-- 'CompileLayer' should be resolved.
newtype SymbolResolver =
  SymbolResolver (MangledSymbol -> IO (Either JITSymbolError JITSymbol))

-- | Create a `FFI.SymbolResolver` that can be used with the JIT.
withSymbolResolver :: ExecutionSession -> SymbolResolver -> (Ptr FFI.SymbolResolver -> IO a) -> IO a
withSymbolResolver (ExecutionSession es) (SymbolResolver resolverFn) f =
  bracket (FFI.wrapSymbolResolverFn resolverFn') freeHaskellFunPtr $ \resolverPtr ->
    bracket (FFI.createLambdaResolver es resolverPtr) FFI.disposeSymbolResolver $ \resolver ->
      f resolver
  where
    resolverFn' symbol result = do
      setSymbol <- encodeM =<< resolverFn =<< decodeM symbol
      setSymbol result

instance Monad m => EncodeM m JITSymbolFlags FFI.JITSymbolFlags where
  encodeM f = return $ foldr1 (.|.) [
      if a f
         then b
         else 0
    | (a,b) <- [
          (jitSymbolWeak, FFI.jitSymbolFlagsWeak),
          (jitSymbolCommon, FFI.jitSymbolFlagsCommon),
          (jitSymbolAbsolute, FFI.jitSymbolFlagsAbsolute),
          (jitSymbolExported, FFI.jitSymbolFlagsExported)
        ]
    ]

instance Monad m => DecodeM m JITSymbolFlags FFI.JITSymbolFlags where
  decodeM f =
    return $ JITSymbolFlags {
      jitSymbolWeak = FFI.jitSymbolFlagsWeak .&. f /= 0,
      jitSymbolCommon = FFI.jitSymbolFlagsCommon .&. f /= 0,
      jitSymbolAbsolute = FFI.jitSymbolFlagsAbsolute .&. f /= 0,
      jitSymbolExported = FFI.jitSymbolFlagsExported .&. f /= 0
    }

instance MonadIO m => EncodeM m (Either JITSymbolError JITSymbol) (Ptr FFI.JITSymbol -> IO ()) where
  encodeM (Left (JITSymbolError _)) = return $ \jitSymbol ->
    FFI.setJITSymbol jitSymbol (FFI.TargetAddress 0) FFI.jitSymbolFlagsHasError
  encodeM (Right (JITSymbol addr flags)) = return $ \jitSymbol -> do
    flags' <- encodeM flags
    FFI.setJITSymbol jitSymbol (FFI.TargetAddress (fromIntegral addr)) flags'

instance (MonadIO m, MonadAnyCont IO m) => DecodeM m (Either JITSymbolError JITSymbol) (Ptr FFI.JITSymbol) where
  decodeM jitSymbol = do
    errMsg <- alloca
    FFI.TargetAddress addr <- liftIO $ FFI.getAddress jitSymbol errMsg
    rawFlags <- liftIO (FFI.getFlags jitSymbol)
    if addr == 0 || (rawFlags .&. FFI.jitSymbolFlagsHasError /= 0)
      then do
        errMsg' <- decodeM errMsg
        pure (Left (JITSymbolError errMsg'))
      else do
        flags <- decodeM rawFlags
        pure (Right (JITSymbol (fromIntegral addr) flags))

instance MonadIO m =>
  EncodeM m SymbolResolver (IORef [IO ()] -> Ptr FFI.ExecutionSession -> IO (Ptr FFI.SymbolResolver)) where
  encodeM (SymbolResolver resolverFn) = return $ \cleanups es -> do
    resolverFn' <- allocFunPtr cleanups (encodeM resolverFn)
    allocWithCleanup cleanups (FFI.createLambdaResolver es resolverFn') FFI.disposeSymbolResolver

instance MonadIO m => EncodeM m (MangledSymbol -> IO (Either JITSymbolError JITSymbol)) (FunPtr FFI.SymbolResolverFn) where
  encodeM callback =
    liftIO $ FFI.wrapSymbolResolverFn
      (\symbol result -> do
         setSymbol <- encodeM =<< callback =<< decodeM symbol
         setSymbol result)

-- | Allocate the resource and register it for cleanup.
allocWithCleanup :: IORef [IO ()] -> IO a -> (a -> IO ()) -> IO a
allocWithCleanup cleanups alloc free = mask $ \restore -> do
  a <- restore alloc
  modifyIORef cleanups (free a :)
  pure a

-- | Allocate a function pointer and register it for cleanup.
allocFunPtr :: IORef [IO ()] -> IO (FunPtr a) -> IO (FunPtr a)
allocFunPtr cleanups alloc = allocWithCleanup cleanups alloc freeHaskellFunPtr

createRegisteredDataLayout :: (MonadAnyCont IO m) => TargetMachine -> IORef [IO ()] -> m (Ptr FFI.DataLayout)
createRegisteredDataLayout (TargetMachine tm) cleanups =
  let createDataLayout = do
        dl <- FFI.createTargetDataLayout tm
        modifyIORef' cleanups (FFI.disposeDataLayout dl :)
        pure dl
  in anyContToM $ bracketOnError createDataLayout FFI.disposeDataLayout

-- | Create a new `ExecutionSession`.
createExecutionSession :: IO ExecutionSession
createExecutionSession = ExecutionSession <$> FFI.createExecutionSession

-- | Dispose of an `ExecutionSession`. This should be called when the
-- `ExecutionSession` is not needed anymore.
disposeExecutionSession :: ExecutionSession -> IO ()
disposeExecutionSession (ExecutionSession es) = FFI.disposeExecutionSession es

-- | `bracket`-style wrapper around `createExecutionSession` and
-- `disposeExecutionSession`.
withExecutionSession :: (ExecutionSession -> IO a) -> IO a
withExecutionSession = bracket createExecutionSession disposeExecutionSession

-- | Allocate a module key for a new module to add to the JIT.
allocateModuleKey :: ExecutionSession -> IO FFI.ModuleKey
allocateModuleKey (ExecutionSession es) = FFI.allocateVModule es

-- | Return a module key to the `ExecutionSession` so that it can be
-- re-used.
releaseModuleKey :: ExecutionSession -> FFI.ModuleKey -> IO ()
releaseModuleKey (ExecutionSession es) k = FFI.releaseVModule es k

-- | `bracket`-style wrapper around `allocateModuleKey` and
-- `releaseModuleKey`.
withModuleKey :: ExecutionSession -> (FFI.ModuleKey -> IO a) -> IO a
withModuleKey es = bracket (allocateModuleKey es) (releaseModuleKey es)
