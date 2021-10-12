{-# LANGUAGE
  MultiParamTypeClasses,
  FunctionalDependencies,
  OverloadedStrings,
  RankNTypes
  #-}
module LLVM.Internal.ExecutionEngine where

import LLVM.Prelude

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.AnyCont

import Data.IORef
import Foreign.Ptr
import Foreign.C (CUInt, CString)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.ExecutionEngine as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

import LLVM.Internal.Module
import LLVM.Internal.Context
import LLVM.Internal.Coding
import qualified LLVM.CodeModel as CodeModel
import LLVM.Internal.Target
import qualified LLVM.AST as A
import GHC.Stack

removeModule :: Ptr FFI.ExecutionEngine -> Ptr FFI.Module -> IO ()
removeModule e m = flip runAnyContT return $ do
  d0 <- alloca
  d1 <- alloca
  r <- liftIO $ FFI.removeModule e m d0 d1
  when (r /= 0) $ fail "FFI.removeModule failure"

-- | a 'ExecutableModule' e represents a 'Module' which is currently "in" an
-- 'ExecutionEngine', and so the functions of which may be executed.
data ExecutableModule e = ExecutableModule e (Ptr FFI.Module)

-- | <http://llvm.org/doxygen/classllvm_1_1ExecutionEngine.html>
class ExecutionEngine e f | e -> f where
  withModuleInEngine :: e -> Module -> (ExecutableModule e -> IO a) -> IO a
  getFunction :: HasCallStack => ExecutableModule e -> A.Name -> IO (Maybe f)

instance ExecutionEngine (Ptr FFI.ExecutionEngine) (FunPtr ()) where
  withModuleInEngine e m f = do
    m' <- readModule m
    bracket_ (FFI.addModule e m') (removeModule e m') (f (ExecutableModule e m'))
  getFunction (ExecutableModule e m) (A.Name name) = flip runAnyContT return $ do
    name <- encodeM name
    f <- liftIO $ FFI.getNamedFunction m name
    if f == nullPtr 
      then 
        return Nothing
      else
        do
          p <- liftIO $ FFI.getPointerToGlobal e (FFI.upCast f)
          return $ if p == nullPtr then Nothing else Just (castPtrToFunPtr p)
  getFunction _ _ = error "Only named functions can be looked up"

withExecutionEngine :: 
  Context ->
  Maybe (Ptr FFI.Module) -> 
  (Ptr (Ptr FFI.ExecutionEngine) -> Ptr FFI.Module -> Ptr (FFI.OwnerTransfered CString) -> IO CUInt) ->
  (Ptr FFI.ExecutionEngine -> IO a) ->
  IO a
withExecutionEngine c m createEngine f = flip runAnyContT return $ do
  liftIO initializeNativeTarget
  outExecutionEngine <- alloca
  outErrorCStringPtr <- alloca
  dummyModule <- maybe (anyContToM $
                          withModuleFromAST c (A.Module "" "" Nothing Nothing []))
                 (liftIO . newModule) m
  dummyModule' <- readModule dummyModule
  r <- liftIO $ createEngine outExecutionEngine dummyModule' outErrorCStringPtr
  when (r /= 0) $ fail =<< decodeM outErrorCStringPtr
  executionEngine <- anyContToM $ bracket (peek outExecutionEngine) FFI.disposeExecutionEngine
  liftIO $ removeModule executionEngine dummyModule'
  liftIO $ f executionEngine

data MCJITState
  = Deferred (forall a . Module -> (Ptr FFI.ExecutionEngine -> IO a) -> IO a)
  | Constructed (Ptr FFI.ExecutionEngine)

-- | <http://llvm.org/doxygen/classllvm_1_1MCJIT.html>
-- <http://blog.llvm.org/2010/04/intro-to-llvm-mc-project.html>
-- N.B. - the LLVM MCJIT does not current support adding multiple
-- modules to any one instance of the MCJIT.
newtype MCJIT = MCJIT (IORef MCJITState)

-- | bracket the creation and destruction of an 'MCJIT'
withMCJIT :: 
  Context
  -> Maybe Word -- ^ optimization level
  -> Maybe CodeModel.Model
  -> Maybe Bool -- ^ True to disable frame pointer elimination
  -> Maybe Bool -- ^ True to enable fast instruction selection
--  -> Maybe MemoryManager -- llvm-hs doesn't support this yet
  -> (MCJIT -> IO a)
  -> IO a
withMCJIT c opt cm fpe fisel f = do
  FFI.linkInMCJIT
  let createMCJITCompilerForModule e m s = do
        size <- FFI.getMCJITCompilerOptionsSize
        allocaBytes (fromIntegral size) $ \p -> do
          FFI.initializeMCJITCompilerOptions p size
          traverse_ (FFI.setMCJITCompilerOptionsOptLevel p <=< encodeM) opt
          traverse_ (FFI.setMCJITCompilerOptionsCodeModel p <=< encodeM) cm
          traverse_ (FFI.setMCJITCompilerOptionsNoFramePointerElim p <=< encodeM) fpe
          traverse_ (FFI.setMCJITCompilerOptionsEnableFastISel p <=< encodeM) fisel
          FFI.createMCJITCompilerForModule e m p size s
  t <- newIORef (Deferred $ \mod f -> do m' <- readModule mod
                                         withExecutionEngine c (Just m') createMCJITCompilerForModule f)
  f (MCJIT t)

instance ExecutionEngine MCJIT (FunPtr ()) where
  withModuleInEngine (MCJIT s) m f = do
    jitState <- readIORef s
    let f' (ExecutableModule _ m) = f (ExecutableModule (MCJIT s) m)
    case jitState of
      Deferred c -> c m $ \e -> 
        bracket_ 
         (writeIORef s (Constructed e))
         (writeIORef s jitState)
         (withModuleInEngine e m f')
      Constructed e -> withModuleInEngine e m f'

  getFunction (ExecutableModule (MCJIT r) m) n = do
    s <- liftIO $ readIORef r
    case s of
      Deferred _ -> return Nothing
      Constructed e -> getFunction (ExecutableModule e m) n
