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

type SymbolResolverFn = MangledSymbol -> IO (Either JITSymbolError JITSymbol)

-- | Specifies how external symbols in a module added to a
-- 'CompielLayer' should be resolved.
data SymbolResolver =
  SymbolResolver {
    -- | This is used to find symbols in the same logical dynamic
    -- library as the module referencing them.
    dylibResolver :: !SymbolResolverFn,
    -- | When 'dylibResolver' fails to resolve a symbol,
    -- 'externalResolver' is used as a fallback to find external symbols.
    externalResolver :: !SymbolResolverFn
  }

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
        errMsg <- decodeM =<< liftIO (FFI.getErrorMsg jitSymbol)
        pure (Left (JITSymbolError errMsg))
      else do
        flags <- decodeM rawFlags
        pure (Right (JITSymbol (fromIntegral addr) flags))

instance MonadIO m =>
  EncodeM m SymbolResolver (IORef [IO ()] -> IO (Ptr FFI.LambdaResolver)) where
  encodeM (SymbolResolver dylib external) = return $ \cleanups -> do
    dylib' <- allocFunPtr cleanups (encodeM dylib)
    external' <- allocFunPtr cleanups (encodeM external)
    allocWithCleanup cleanups (FFI.createLambdaResolver dylib' external') FFI.disposeLambdaResolver

instance MonadIO m => EncodeM m SymbolResolverFn (FunPtr FFI.SymbolResolverFn) where
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
