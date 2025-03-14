-- | This module provides an interface to LLVM's passes.
module LLVM.Internal.Passes where

import LLVM.Prelude

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr

import LLVM.Internal.Module
import LLVM.Internal.Target

import qualified LLVM.Internal.FFI.Passes as FFI

data ModulePass
  = GlobalDeadCodeElimination
  | InternalizeFunctions { exportList :: [String] }
  | AlwaysInline { insertLifetime :: Bool }
  | CuratedPassSet { optLevel :: Word }

data PassSetSpec
  = PassSetSpec {
      passes :: [ModulePass],
      targetMachine :: Maybe TargetMachine
    }

runPasses :: PassSetSpec -> Module -> IO ()
runPasses (PassSetSpec passes tm) m = do
  m' <- readModule m
  pb <- FFI.createPassBuilderPackage tm'
  mpm <- FFI.createModulePassManager
  forM_ passes $ addPass mpm pb
  FFI.modulePassManagerRun mpm pb m'
  FFI.disposeModulePassManager mpm
  FFI.disposePassBuilderPackage pb
  where tm' = case tm of Nothing -> nullPtr; Just (TargetMachine ptr) -> ptr

addPass :: Ptr FFI.ModulePassManager -> Ptr FFI.PassBuilderPackage -> ModulePass -> IO ()
addPass mpm pb p = case p of
  CuratedPassSet level -> FFI.addPerModuleDefaultPipeline mpm pb (fromIntegral level)
  GlobalDeadCodeElimination -> FFI.addGlobalDeadCodeEliminationPass mpm
  AlwaysInline l -> FFI.addAlwaysInlinePass mpm (if l then 1 else 0)
  InternalizeFunctions exports -> encodeExports exports $ FFI.addInternalizeFunctionsPass mpm

encodeExports :: [String] -> (CInt -> Ptr CString -> IO a) -> IO a
encodeExports topExports cont = go [] topExports
  where
    go ptrs exports = case exports of
      [] -> withArrayLen ptrs $ \n p -> cont (fromIntegral n) p
      (e:es) -> withCString e $ \ep -> go (ep:ptrs) es
