module LLVM.General.Internal.Function where

import LLVM.General.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont

import Foreign.C (CUInt)
import Foreign.Ptr  

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.General.Internal.FFI.Function as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.Value
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Attribute

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.ParameterAttribute as A.PA  

getMixedAttributeSet :: Ptr FFI.Function -> DecodeAST MixedAttributeSet
getMixedAttributeSet = decodeM <=< liftIO . FFI.getMixedAttributeSet

setFunctionAttributes :: Ptr FFI.Function -> MixedAttributeSet -> EncodeAST ()
setFunctionAttributes f = (liftIO . FFI.setMixedAttributeSet f) <=< encodeM

getParameters :: Ptr FFI.Function -> Map CUInt [A.PA.ParameterAttribute] -> DecodeAST [A.Parameter]
getParameters f attrs = scopeAnyCont $ do
  n <- liftIO (FFI.countParams f)
  ps <- allocaArray n
  liftIO $ FFI.getParams f ps
  params <- peekArray n ps
  forM (zip params [0..]) $ \(param, i) -> 
    return A.Parameter 
       `ap` typeOf param
       `ap` getLocalName param
       `ap` (return $ Map.findWithDefault [] i attrs)
  
getGC :: Ptr FFI.Function -> DecodeAST (Maybe String)
getGC f = scopeAnyCont $ decodeM =<< liftIO (FFI.getGC f)

setGC :: Ptr FFI.Function -> Maybe String -> EncodeAST ()
setGC f gc = scopeAnyCont $ liftIO . FFI.setGC f =<< encodeM gc 
