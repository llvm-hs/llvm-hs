module LLVM.General.Internal.Function where

import LLVM.General.Prelude

import Control.Monad.Trans
import Control.Monad.AnyCont
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.Attributes as FFI
import qualified LLVM.General.Internal.FFI.Function as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.Value
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Attribute ()

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.Attribute as A.A

getCombinedAttributeSet :: Ptr FFI.Function -> DecodeAST
                           ([Either A.A.GroupID A.A.FunctionAttribute], [A.A.ParameterAttribute], Map Int [A.A.ParameterAttribute])
getCombinedAttributeSet p = do
  c <- liftIO $ FFI.getCombinedAttributeSet p
  numSlots <- if c == nullPtr then return 0 else liftIO $ FFI.attributeSetNumSlots c
  slotIndexes <- forM (take (fromIntegral numSlots) [0..]) $ \s -> do
    i <- liftIO $ FFI.attributeSetSlotIndex c s
    return (i, s)
  let separate :: Ord k => k -> Map k a -> (Maybe a, Map k a)
      separate = Map.updateLookupWithKey (\_ _ -> Nothing)
      indexedSlots = Map.fromList slotIndexes
  unless (Map.size indexedSlots == length slotIndexes) $
         fail "unexpected slot index collision decoding combined AttributeSet"
  let (functionSlot, otherSlots) = separate FFI.functionIndex (Map.fromList slotIndexes)
  functionAnnotation <- for (maybeToList functionSlot) $ \slot -> do
    a <- liftIO $ FFI.attributeSetSlotAttributes c slot
    getAttributeGroupID a
  otherAttributeSets <- for otherSlots $ \slot -> do
    a <- liftIO $ FFI.attributeSetSlotAttributes c slot
    decodeM (a :: FFI.ParameterAttributeSet)
  let (returnAttributeSet, shiftedParameterAttributeSets) = separate FFI.returnIndex otherAttributeSets
  return (fmap Left functionAnnotation,
          join . maybeToList $ returnAttributeSet,
          Map.mapKeysMonotonic (\x -> fromIntegral x - 1) shiftedParameterAttributeSets)

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
