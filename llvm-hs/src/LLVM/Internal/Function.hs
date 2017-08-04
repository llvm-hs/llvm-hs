module LLVM.Internal.Function where

import LLVM.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont

import Foreign.Ptr

import qualified LLVM.Internal.FFI.Function as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Attribute as FFI

import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.Value
import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Attribute

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as A
import qualified LLVM.AST.ParameterAttribute as A.PA

getAttributeList :: Ptr FFI.Function -> DecodeAST AttributeList
getAttributeList f = do
  decodeM (FFI.AttrSetDecoder FFI.attributesAtIndex FFI.countParams, f)

setFunctionAttributes :: Ptr FFI.Function -> AttributeList -> EncodeAST ()
setFunctionAttributes f = liftIO . FFI.setAttributeList f <=< encodeM

getParameters :: Ptr FFI.Function -> [[A.PA.ParameterAttribute]] -> DecodeAST [A.Parameter]
getParameters f attrs = scopeAnyCont $ do
  n <- liftIO (FFI.countParams f)
  ps <- allocaArray n
  liftIO $ FFI.getParams f ps
  params <- peekArray n ps
  forM (leftBiasedZip params attrs) $ \(param, pAttrs) ->
    A.Parameter
      <$> typeOf param
      <*> getLocalName param
      <*> (return $ fromMaybe [] pAttrs)
  
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
setPrefixData f = traverse_ (liftIO . FFI.setPrefixData f <=< encodeM)

getPersonalityFn :: Ptr FFI.Function -> DecodeAST (Maybe A.Constant)
getPersonalityFn f = do
  has <- decodeM =<< liftIO (FFI.hasPersonalityFn f)
  if has
     then decodeM =<< liftIO (FFI.getPersonalityFn f)
     else pure Nothing

setPersonalityFn :: Ptr FFI.Function -> Maybe A.Constant -> EncodeAST ()
setPersonalityFn f personality = (liftIO . FFI.setPersonalityFn f =<< encodeM personality)
