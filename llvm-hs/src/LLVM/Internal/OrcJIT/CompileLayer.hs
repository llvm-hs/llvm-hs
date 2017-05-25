module LLVM.Internal.OrcJIT.CompileLayer
  ( module LLVM.Internal.OrcJIT.CompileLayer
  , FFI.ModuleSetHandle
  ) where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr

import LLVM.Internal.Coding
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import LLVM.Internal.Module hiding (getDataLayout)
import LLVM.Internal.OrcJIT

-- | There are two main types of operations provided by instances of 'CompileLayer'.
--
-- 1. You can add \/ remove modules using 'addModuleSet' \/ 'removeModuleSet'.
--
-- 2. You can search for symbols using 'findSymbol' \/ 'findSymbolIn' in
-- the previously added modules.
class CompileLayer l where
  getCompileLayer :: l -> Ptr FFI.CompileLayer
  getDataLayout :: l -> Ptr FFI.DataLayout
  getCleanups :: l -> IORef [IO ()]

-- | Mangle a symbol according to the data layout stored in the
-- 'CompileLayer'.
mangleSymbol :: CompileLayer l => l -> ShortByteString -> IO MangledSymbol
mangleSymbol compileLayer symbol = flip runAnyContT return $ do
  mangledSymbol <- alloca
  symbol' <- encodeM symbol
  anyContToM $ bracket
    (FFI.getMangledSymbol mangledSymbol symbol' (getDataLayout compileLayer))
    (\_ -> FFI.disposeMangledSymbol =<< peek mangledSymbol)
  decodeM =<< peek mangledSymbol

-- | @'findSymbol' layer symbol exportedSymbolsOnly@ searches for
-- @symbol@ in all modules added to @layer@. If @exportedSymbolsOnly@
-- is 'True' only exported symbols are searched.
findSymbol :: CompileLayer l => l -> MangledSymbol -> Bool -> IO JITSymbol
findSymbol compileLayer symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol (getCompileLayer compileLayer) symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

-- | @'findSymbolIn' layer handle symbol exportedSymbolsOnly@ searches for
-- @symbol@ in the context of the modules represented by @handle@. If
-- @exportedSymbolsOnly@ is 'True' only exported symbols are searched.
findSymbolIn :: CompileLayer l => l -> FFI.ModuleSetHandle -> MangledSymbol -> Bool -> IO JITSymbol
findSymbolIn compileLayer handle symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbolIn (getCompileLayer compileLayer) handle symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

-- | Add a list of modules to the 'CompileLayer'. The 'SymbolResolver' is used
-- to resolve external symbols in these modules.
--
-- /Note:/ This function consumes the modules passed be it and they
-- must not be used after calling this method.
addModuleSet :: CompileLayer l => l -> [Module] -> SymbolResolver -> IO FFI.ModuleSetHandle
addModuleSet compileLayer modules resolver = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver' <- liftIO $ resolverAct (getCleanups compileLayer)
  modules' <- liftIO $ mapM readModule modules
  liftIO $ mapM_ deleteModule modules
  (moduleCount, modules'') <-
    anyContToM $ \f -> withArrayLen modules' $ \n hs -> f (fromIntegral n, hs)
  liftIO $
    FFI.addModuleSet
      (getCompileLayer compileLayer)
      (getDataLayout compileLayer)
      modules''
      moduleCount
      resolver'

-- | Remove a set of previously added modules.
removeModuleSet :: CompileLayer l => l -> FFI.ModuleSetHandle -> IO ()
removeModuleSet compileLayer handle =
  FFI.removeModuleSet (getCompileLayer compileLayer) handle

-- | 'bracket'-style wrapper around 'addModuleSet' and 'removeModuleSet'.
--
-- /Note:/ This function consumes the modules passed to it and they
-- must not be used after calling this method.
withModuleSet :: CompileLayer l => l -> [Module] -> SymbolResolver -> (FFI.ModuleSetHandle -> IO a) -> IO a
withModuleSet compileLayer modules resolver =
  bracket
    (addModuleSet compileLayer modules resolver)
    (removeModuleSet compileLayer)

-- | Dispose a 'CompileLayer'. This should called when the
-- 'CompileLayer' is not needed anymore.
disposeCompileLayer :: CompileLayer l => l -> IO ()
disposeCompileLayer l = FFI.disposeCompileLayer (getCompileLayer l)
