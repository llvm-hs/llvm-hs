module LLVM.Internal.Function where

import LLVM.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont

import Foreign.C (CUInt)
import Foreign.Ptr  

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.Internal.FFI.Function as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.Value
import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Attribute

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as A
import qualified LLVM.AST.ParameterAttribute as A.PA  

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
  
getGC :: Ptr FFI.Function -> DecodeAST (Maybe ShortByteString)
getGC f = scopeAnyCont $ decodeM =<< liftIO (FFI.getGC f)

setGC :: Ptr FFI.Function -> Maybe ShortByteString -> EncodeAST ()
setGC f gc = scopeAnyCont $ liftIO . FFI.setGC f =<< encodeM gc 

getPrefixData :: Ptr FFI.Function -> DecodeAST (Maybe A.Constant)
getPrefixData f = do
  has <- decodeM =<< (liftIO $ FFI.hasPrefixData f)
  if has
   then decodeM =<< (liftIO $ FFI.getPrefixData f)
   else return Nothing

setPrefixData :: Ptr FFI.Function -> Maybe A.Constant -> EncodeAST ()
setPrefixData f = maybe (return ()) (liftIO . FFI.setPrefixData f <=< encodeM)

getPersonalityFn :: Ptr FFI.Function -> DecodeAST (Maybe A.Constant)
getPersonalityFn f = do
  has <- decodeM =<< liftIO (FFI.hasPersonalityFn f)
  if has
     then decodeM =<< liftIO (FFI.getPersonalityFn f)
     else pure Nothing

setPersonalityFn :: Ptr FFI.Function -> Maybe A.Constant -> EncodeAST ()
setPersonalityFn f personality = (liftIO . FFI.setPersonalityFn f =<< encodeM personality)
