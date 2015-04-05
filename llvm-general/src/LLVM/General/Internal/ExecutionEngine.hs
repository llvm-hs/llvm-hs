{-# LANGUAGE
  MultiParamTypeClasses,
  FunctionalDependencies,
  RankNTypes
  #-}
module LLVM.General.Internal.ExecutionEngine where

import LLVM.General.Prelude

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.AnyCont
import Control.Monad.Exceptable

import Data.IORef
import Foreign.Ptr
import Foreign.C (CUInt, CString)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.ExecutionEngine as FFI
import qualified LLVM.General.Internal.FFI.Module as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import qualified LLVM.General.CodeModel as CodeModel
import LLVM.General.Internal.Target
import qualified LLVM.General.AST as A

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
  getFunction :: ExecutableModule e -> A.Name -> IO (Maybe f)

instance ExecutionEngine (Ptr FFI.ExecutionEngine) (FunPtr ()) where
  withModuleInEngine e (Module m) = bracket_ (FFI.addModule e m) (removeModule e m) . ($ (ExecutableModule e m)) 
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
  Module dummyModule <- maybe (anyContToM $ liftM (either undefined id) . runExceptableT . ExceptableT
                                   . withModuleFromAST c (A.Module "" Nothing Nothing []))
                        (return . Module) m
  r <- liftIO $ createEngine outExecutionEngine dummyModule outErrorCStringPtr
  when (r /= 0) $ fail =<< decodeM outErrorCStringPtr
  executionEngine <- anyContToM $ bracket (peek outExecutionEngine) FFI.disposeExecutionEngine
  liftIO $ removeModule executionEngine dummyModule
  liftIO $ f executionEngine
          
-- | <http://llvm.org/doxygen/classllvm_1_1JIT.html>
newtype JIT = JIT (Ptr FFI.ExecutionEngine)

-- | bracket the creation and destruction of a 'JIT'
withJIT :: 
  Context
  -> Word -- ^ optimization level
  -> (JIT -> IO a)
  -> IO a
withJIT c opt f = FFI.linkInJIT >> withJIT' f
  where withJIT' =
         withExecutionEngine c Nothing (\e m -> FFI.createJITCompilerForModule e m (fromIntegral opt))
         . (. JIT)

instance ExecutionEngine JIT (FunPtr ()) where
  withModuleInEngine (JIT e) m f = withModuleInEngine e m (\(ExecutableModule e m) -> f (ExecutableModule (JIT e) m))
  getFunction (ExecutableModule (JIT e) m) = getFunction (ExecutableModule e m)
      

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
--  -> Maybe MemoryManager -- llvm-general doesn't support this yet
  -> (MCJIT -> IO a)
  -> IO a
withMCJIT c opt cm fpe fisel f = do
  FFI.linkInMCJIT
  let createMCJITCompilerForModule e m s = do
        size <- FFI.getMCJITCompilerOptionsSize
        allocaBytes (fromIntegral size) $ \p -> do
          FFI.initializeMCJITCompilerOptions p size
          maybe (return ()) (FFI.setMCJITCompilerOptionsOptLevel p <=< encodeM) opt
          maybe (return ()) (FFI.setMCJITCompilerOptionsCodeModel p <=< encodeM) cm
          maybe (return ()) (FFI.setMCJITCompilerOptionsNoFramePointerElim p <=< encodeM) fpe
          maybe (return ()) (FFI.setMCJITCompilerOptionsEnableFastISel p <=< encodeM) fisel
          FFI.createMCJITCompilerForModule e m p size s
  t <- newIORef (Deferred $ \(Module m) -> withExecutionEngine c (Just m) createMCJITCompilerForModule)
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
