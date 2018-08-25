module LLVM.Internal.OrcJIT.CompileLayer
  ( module LLVM.Internal.OrcJIT.CompileLayer
  , FFI.ModuleKey
  ) where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import LLVM.Internal.Coding
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import LLVM.Internal.Module hiding (getDataLayout)
import LLVM.Internal.OrcJIT

-- | There are two main types of operations provided by instances of 'CompileLayer'.
--
-- 1. You can add \/ remove modules using 'addModule' \/ 'removeModuleSet'.
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
findSymbol :: CompileLayer l => l -> MangledSymbol -> Bool -> IO (Either JITSymbolError JITSymbol)
findSymbol compileLayer symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol (getCompileLayer compileLayer) symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

-- | @'findSymbolIn' layer handle symbol exportedSymbolsOnly@ searches for
-- @symbol@ in the context of the module represented by @handle@. If
-- @exportedSymbolsOnly@ is 'True' only exported symbols are searched.
findSymbolIn :: CompileLayer l => l -> FFI.ModuleKey -> MangledSymbol -> Bool -> IO (Either JITSymbolError JITSymbol)
findSymbolIn compileLayer handle symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbolIn (getCompileLayer compileLayer) handle symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

-- | Add a module to the 'CompileLayer'. The 'SymbolResolver' is used
-- to resolve external symbols in the module.
--
-- /Note:/ This function consumes the module passed to it and it must
-- not be used after calling this method.
addModule :: CompileLayer l => l -> FFI.ModuleKey -> Module -> IO ()
addModule compileLayer k mod = flip runAnyContT return $ do
  mod' <- liftIO $ readModule mod
  liftIO $ deleteModule mod
  errMsg <- alloca
  liftIO $
    FFI.addModule
      (getCompileLayer compileLayer)
      (getDataLayout compileLayer)
      k
      mod'
      errMsg

-- | Remove a previously added module.
removeModule :: CompileLayer l => l -> FFI.ModuleKey -> IO ()
removeModule compileLayer handle =
  FFI.removeModule (getCompileLayer compileLayer) handle

-- | 'bracket'-style wrapper around 'addModule' and 'removeModule'.
--
-- /Note:/ This function consumes the module passed to it and it must
-- not be used after calling this method.
withModule :: CompileLayer l => l -> FFI.ModuleKey -> Module -> IO a -> IO a
withModule compileLayer k mod =
  bracket_
    (addModule compileLayer k mod)
    (removeModule compileLayer k)

-- | Dispose of a 'CompileLayer'. This should called when the
-- 'CompileLayer' is not needed anymore.
disposeCompileLayer :: CompileLayer l => l -> IO ()
disposeCompileLayer l = do
  FFI.disposeCompileLayer (getCompileLayer l)
  sequence_ =<< readIORef (getCleanups l)
