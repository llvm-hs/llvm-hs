module LLVM.General.Internal.Function where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.AnyCont

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.Function as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.Value
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Attribute ()

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Attribute as A.A

getFunctionAttrs :: Ptr FFI.Function -> DecodeAST [Either A.A.GroupID A.A.FunctionAttribute]
getFunctionAttrs p = do
  a <- liftIO $ FFI.getFunctionAttr p
  case a of
    0 -> return []
    a -> do
      gid <- getAttributeGroupID a
      return [Left gid]

setFunctionAttrs :: Ptr FFI.Function -> [Either A.A.GroupID A.A.FunctionAttribute] -> EncodeAST ()
setFunctionAttrs f = (liftIO . FFI.addFunctionAttr f) <=< encodeM 

getParameterAttrs :: Ptr FFI.Parameter -> IO [A.A.ParameterAttribute]
getParameterAttrs = decodeM <=< FFI.getAttribute

getParameters :: Ptr FFI.Function -> DecodeAST [A.Parameter]
getParameters f = scopeAnyCont $ do
  n <- liftIO (FFI.countParams f)
  ps <- allocaArray n
  liftIO $ FFI.getParams f ps
  params <- peekArray n ps
  forM params $ \param -> 
    return A.Parameter 
       `ap` typeOf param
       `ap` getLocalName param
       `ap` (liftIO $ getParameterAttrs param)
  
getGC :: Ptr FFI.Function -> DecodeAST (Maybe String)
getGC f = scopeAnyCont $ decodeM =<< liftIO (FFI.getGC f)

setGC :: Ptr FFI.Function -> Maybe String -> EncodeAST ()
setGC f gc = scopeAnyCont $ liftIO . FFI.setGC f =<< encodeM gc 
