{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  UndecidableInstances,
  OverloadedStrings
  #-}
module LLVM.Internal.DecodeAST where

import LLVM.Prelude

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.AnyCont

import Foreign.Ptr
import Foreign.C

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array (Array)
import qualified Data.Array as Array

import qualified LLVM.Internal.FFI.Attribute as FFI
import qualified LLVM.Internal.FFI.GlobalValue as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI
import qualified LLVM.Internal.FFI.Type as FFI

import qualified LLVM.AST.Name as A
import qualified LLVM.AST.Operand as A (MetadataNodeID(..))
import qualified LLVM.AST.Attribute as A.A
import qualified LLVM.AST.COMDAT as A.COMDAT

import LLVM.Internal.Coding
import LLVM.Internal.String ()

type NameMap a = Map (Ptr a) Word

data DecodeState = DecodeState {
    globalVarNum :: NameMap FFI.GlobalValue,
    localVarNum :: NameMap FFI.Value,
    localNameCounter :: Maybe Word,
    namedTypeNum :: NameMap FFI.Type,
    typesToDefine :: Seq (Ptr FFI.Type),
    metadataNodesToDefine :: Seq (A.MetadataNodeID, Ptr FFI.MDNode),
    metadataNodes :: Map (Ptr FFI.MDNode) A.MetadataNodeID,
    metadataKinds :: Array Word ShortByteString,
    parameterAttributeLists :: Map FFI.ParameterAttributeSet [A.A.ParameterAttribute],
    functionAttributeListIDs :: [(FFI.FunctionAttributeSet, A.A.GroupID)],
    comdats :: Map (Ptr FFI.COMDAT) (ShortByteString, A.COMDAT.SelectionKind)
  }

initialDecode :: DecodeState
initialDecode = DecodeState {
    globalVarNum = Map.empty,
    localVarNum = Map.empty,
    localNameCounter = Nothing,
    namedTypeNum = Map.empty,
    typesToDefine = Seq.empty,
    metadataNodesToDefine = Seq.empty,
    metadataNodes = Map.empty,
    metadataKinds = Array.listArray (1,0) [],
    parameterAttributeLists = Map.empty,
    functionAttributeListIDs = [],
    comdats = Map.empty
  }
newtype DecodeAST a = DecodeAST { unDecodeAST :: AnyContT (StateT DecodeState IO) a }
  deriving (
    Applicative,
    Functor,
    Monad,
    MonadIO,
    MonadFail,
    MonadState DecodeState,
    MonadThrow,
    MonadAnyCont IO,
    ScopeAnyCont
  )

runDecodeAST :: DecodeAST a -> IO a
runDecodeAST d = flip evalStateT initialDecode . flip runAnyContT return . unDecodeAST $ d

localScope :: DecodeAST a -> DecodeAST a
localScope (DecodeAST x) = DecodeAST (tweak x)
  where tweak x = do
          modify (\s@DecodeState { localNameCounter = Nothing } -> s { localNameCounter = Just 0 })
          r <- x
          modify (\s@DecodeState { localNameCounter = Just _ } -> s { localNameCounter = Nothing })
          return r

getName :: (Ptr a -> IO CString)
           -> Ptr a
           -> (DecodeState -> NameMap a)
           -> DecodeAST Word
           -> DecodeAST A.Name
getName getCString v getNameMap generate = do
  name <- liftIO $ do
            n <- getCString v
            if n == nullPtr then return "" else decodeM n
  if name /= ""
     then
       return $ A.Name name
     else
       A.UnName <$> do
         nm <- gets getNameMap
         maybe generate return $ Map.lookup v nm

getValueName :: FFI.DescendentOf FFI.Value v => Ptr v -> (DecodeState -> NameMap v) -> DecodeAST Word -> DecodeAST A.Name
getValueName = getName (FFI.getValueName . FFI.upCast)

getLocalName :: FFI.DescendentOf FFI.Value v => Ptr v -> DecodeAST A.Name
getLocalName v' = do
  let v = FFI.upCast v'
  getValueName v localVarNum $ do
                    nm <- gets localVarNum
                    Just n <- gets localNameCounter
                    modify $ \s -> s { localNameCounter = Just (1 + n), localVarNum = Map.insert v n nm }
                    return n

getGlobalName :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST A.Name
getGlobalName v' = do
  let v = FFI.upCast v'
  getValueName v globalVarNum $ do
                     nm <- gets globalVarNum
                     let n = fromIntegral $ Map.size nm
                     modify $ \s -> s { globalVarNum = Map.insert v n nm }
                     return n


getTypeName :: Ptr FFI.Type -> DecodeAST A.Name
getTypeName t = do
  getName FFI.getStructName t namedTypeNum $ do
                  nm <- gets namedTypeNum
                  let n = fromIntegral $ Map.size nm
                  modify $ \s -> s { namedTypeNum = Map.insert t n nm }
                  return n

saveNamedType :: Ptr FFI.Type -> DecodeAST ()
saveNamedType t = do
  modify $ \s -> s { typesToDefine = t Seq.<| typesToDefine s }

getMetadataNodeID :: Ptr FFI.MDNode -> DecodeAST A.MetadataNodeID
getMetadataNodeID p = do
  mdns <- gets metadataNodes
  case Map.lookup p mdns of
    Just r -> return r
    Nothing -> do
      let r = A.MetadataNodeID (fromIntegral (Map.size mdns))
      modify $ \s -> s {
        metadataNodesToDefine = (r, p) Seq.<| metadataNodesToDefine s,
        metadataNodes = Map.insert p r (metadataNodes s)
      }
      return r

takeTypeToDefine :: DecodeAST (Maybe (Ptr FFI.Type))
takeTypeToDefine = state $ \s -> case Seq.viewr (typesToDefine s) of
  remaining Seq.:> t -> (Just t, s { typesToDefine = remaining })
  _ -> (Nothing, s)

takeMetadataNodeToDefine :: DecodeAST (Maybe (A.MetadataNodeID, Ptr FFI.MDNode))
takeMetadataNodeToDefine = state $ \s -> case Seq.viewr (metadataNodesToDefine s) of
  remaining Seq.:> md -> (Just md, s { metadataNodesToDefine = remaining })
  _ -> (Nothing, s)

instance DecodeM DecodeAST A.Name (Ptr FFI.BasicBlock) where
  decodeM = getLocalName

getAttributeGroupID :: FFI.FunctionAttributeSet -> DecodeAST A.A.GroupID
getAttributeGroupID p = do
  ids <- gets functionAttributeListIDs
  -- What we are interested in is the AttributeSetNode inside the
  -- AttributeSet but LLVM does not expose this. We thus have to
  -- resort to doing a linear scan and using the operator== which is
  -- implemented as a comparison on those AttributeSetNodes.
  id <- liftIO (findM (\(as, _) -> decodeM =<< FFI.attributeSetsEqual as p) ids)
  case id of
    Nothing -> do
      let r = A.A.GroupID (fromIntegral (length ids))
      modify $ \s -> s { functionAttributeListIDs = (p,r) : functionAttributeListIDs s }
      return r
    Just (_, id') -> do
      liftIO (FFI.disposeAttributeSet p)
      return id'
