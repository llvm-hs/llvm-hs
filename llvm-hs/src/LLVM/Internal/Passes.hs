-- | This module provides an interface to LLVM's passes.
module LLVM.Internal.Passes where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C.String

import LLVM.Internal.Module
import LLVM.Internal.Target

import qualified LLVM.Internal.FFI.Error  as FFI
import qualified LLVM.Internal.FFI.Passes as FFI

data PassSetSpec
  -- | This type is a high-level specification of a set of passes. It uses the same
  -- collection of passes chosen by the LLVM team in the command line tool 'opt'.  The fields
  -- of this spec are much like typical compiler command-line flags - e.g. -O\<n\>, etc.
  = CuratedPassSetSpec {
      optLevel :: Word,
      targetMachine :: Maybe TargetMachine
    }

runPasses :: PassSetSpec -> Module -> IO Bool
runPasses spec m = do
  m' <- readModule m
  opts <- FFI.createPassBuilderOptions
  err <- withCString passStr $ \passCStr ->
    FFI.runPasses m' passCStr tm' opts
  FFI.disposePassBuilderOptions opts
  FFI.consumeError err
  return $ err == nullPtr
  where
    (passStr, tm) = case spec of
      CuratedPassSetSpec optLevel tm -> ("default<O" ++ show optLevel ++ ">", tm)
    tm' = case tm of Nothing -> nullPtr; Just (TargetMachine ptr) -> ptr
