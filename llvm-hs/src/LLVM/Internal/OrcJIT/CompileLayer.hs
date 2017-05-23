module LLVM.Internal.OrcJIT.CompileLayer where

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


class CompileLayer a where
  getCompileLayer :: a -> Ptr FFI.CompileLayer
  getDataLayout :: a -> Ptr FFI.DataLayout
  getCleanups :: a -> IORef [IO ()]

mangleSymbol :: CompileLayer l => l -> ShortByteString -> IO MangledSymbol
mangleSymbol compileLayer symbol = flip runAnyContT return $ do
  mangledSymbol <- alloca
  symbol' <- encodeM symbol
  anyContToM $ bracket
    (FFI.getMangledSymbol mangledSymbol symbol' (getDataLayout compileLayer))
    (\_ -> FFI.disposeMangledSymbol =<< peek mangledSymbol)
  decodeM =<< peek mangledSymbol

findSymbol :: CompileLayer l => l -> MangledSymbol -> Bool -> IO JITSymbol
findSymbol compileLayer symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol (getCompileLayer compileLayer) symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

addModuleSet :: CompileLayer l => l -> [Module] -> SymbolResolver -> IO FFI.ModuleSetHandle
addModuleSet compileLayer modules resolver = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver' <- liftIO $ resolverAct (getCleanups compileLayer)
  modules' <- liftIO $ mapM readModule modules
  (moduleCount, modules'') <-
    anyContToM $ \f -> withArrayLen modules' $ \n hs -> f (fromIntegral n, hs)
  liftIO $
    FFI.addModuleSet
      (getCompileLayer compileLayer)
      (getDataLayout compileLayer)
      modules''
      moduleCount
      resolver'

removeModuleSet :: CompileLayer l => l -> FFI.ModuleSetHandle -> IO ()
removeModuleSet compileLayer handle =
  FFI.removeModuleSet (getCompileLayer compileLayer) handle

withModuleSet :: CompileLayer l => l -> [Module] -> SymbolResolver -> (FFI.ModuleSetHandle -> IO a) -> IO a
withModuleSet compileLayer modules resolver =
  bracket
    (addModuleSet compileLayer modules resolver)
    (removeModuleSet compileLayer)
