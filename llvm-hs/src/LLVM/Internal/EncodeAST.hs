{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  UndecidableInstances,
  OverloadedStrings
  #-}
module LLVM.Internal.EncodeAST where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.State

import Foreign.Ptr
import Foreign.C

import qualified LLVM.Internal.FFI.ShortByteString as ShortByteString
import qualified Data.ByteString.Short as ShortByteString

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.Internal.FFI.Attribute as FFI
import qualified LLVM.Internal.FFI.Builder as FFI
import qualified LLVM.Internal.FFI.GlobalValue as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import qualified LLVM.AST as A
import qualified LLVM.AST.Attribute as A.A
import LLVM.Exception

import LLVM.Internal.Context
import LLVM.Internal.Coding
import LLVM.Internal.String ()

data LocalValue
  = ForwardValue (Ptr FFI.Value)
  | DefinedValue (Ptr FFI.Value)

data EncodeState = EncodeState {
      encodeStateBuilder :: Ptr FFI.Builder,
      encodeStateContext :: Context,
      encodeStateLocals :: Map A.Name LocalValue,
      encodeStateGlobals :: Map A.Name (Ptr FFI.GlobalValue),
      encodeStateAllBlocks :: Map (A.Name, A.Name) (Ptr FFI.BasicBlock),
      encodeStateBlocks :: Map A.Name (Ptr FFI.BasicBlock),
      encodeStateMDNodes :: Map A.MetadataNodeID (Ptr FFI.MDNode),
      encodeStateNamedTypes :: Map A.Name (Ptr FFI.Type),
      encodeStateRenamedTypes :: Map A.Name ShortByteString,
      encodeStateAttributeGroups :: Map A.A.GroupID FFI.FunctionAttributeSet,
      encodeStateCOMDATs :: Map ShortByteString (Ptr FFI.COMDAT)
    }

newtype EncodeAST a = EncodeAST { unEncodeAST :: AnyContT (StateT EncodeState IO) a }
    deriving (
       Functor,
       Applicative,
       Monad,
       MonadIO,
       MonadState EncodeState,
       MonadThrow,
       MonadAnyCont IO,
       ScopeAnyCont
     )

lookupNamedType :: A.Name -> EncodeAST (Ptr FFI.Type)
lookupNamedType n = do
  t <- gets $ Map.lookup n . encodeStateNamedTypes
  maybe (throwM . EncodeException $ "reference to undefined type: " ++ show n) return t

defineType :: A.Name -> Maybe ShortByteString -> Ptr FFI.Type -> EncodeAST ()
defineType n n' t = do
  modify $ \s -> s { encodeStateNamedTypes = Map.insert n t (encodeStateNamedTypes s) }
  for_ n' $ \renamedName ->
    modify $ \s -> s { encodeStateRenamedTypes = Map.insert n renamedName (encodeStateRenamedTypes s) }

runEncodeAST :: Context -> EncodeAST a -> IO a
runEncodeAST context@(Context ctx) (EncodeAST a) =
    bracket (FFI.createBuilderInContext ctx) FFI.disposeBuilder $ \builder -> do
      let initEncodeState = EncodeState {
              encodeStateBuilder = builder,
              encodeStateContext = context,
              encodeStateLocals = Map.empty,
              encodeStateGlobals = Map.empty,
              encodeStateAllBlocks = Map.empty,
              encodeStateBlocks = Map.empty,
              encodeStateMDNodes = Map.empty,
              encodeStateNamedTypes = Map.empty,
              encodeStateRenamedTypes = Map.empty,
              encodeStateAttributeGroups = Map.empty,
              encodeStateCOMDATs = Map.empty
            }
      flip evalStateT initEncodeState . flip runAnyContT return $ a

withName :: A.Name -> (CString -> IO a) -> IO a
withName (A.Name n) = ShortByteString.useAsCString n
withName (A.UnName _) = withCString ""

instance MonadAnyCont IO m => EncodeM m A.Name CString where
  encodeM (A.Name n) = encodeM n
  encodeM _ = encodeM ShortByteString.empty

phase :: EncodeAST a -> EncodeAST (EncodeAST a)
phase p = do
  let s0 `withLocalsFrom` s1 = s0 {
         encodeStateLocals = encodeStateLocals s1,
         encodeStateBlocks = encodeStateBlocks s1
        }
  s <- get
  return $ do
    s' <- get
    put $ s' `withLocalsFrom` s
    r <- p
    modify (`withLocalsFrom` s')
    return r

defineLocal :: FFI.DescendentOf FFI.Value v => A.Name -> Ptr v -> EncodeAST ()
defineLocal n v' = do
  let v = FFI.upCast v'
  case n of
    A.Name s
      | ShortByteString.null s -> pure ()
    _ -> do
      def <- gets $ Map.lookup n . encodeStateLocals
      case def of
        Just (ForwardValue dummy) -> liftIO $ FFI.replaceAllUsesWith dummy v
        Just _ -> throwM (EncodeException ("Duplicate definition of local variable: " <> show n <> "."))
        _ -> return ()
      modify $ \b -> b { encodeStateLocals = Map.insert n (DefinedValue v) (encodeStateLocals b) }

defineGlobal :: FFI.DescendentOf FFI.GlobalValue v => A.Name -> Ptr v -> EncodeAST ()
defineGlobal n v = modify $ \b -> b { encodeStateGlobals =  Map.insert n (FFI.upCast v) (encodeStateGlobals b) }

defineMDNode :: A.MetadataNodeID -> Ptr FFI.MDNode -> EncodeAST ()
defineMDNode n v = modify $ \b -> b { encodeStateMDNodes = Map.insert n (FFI.upCast v) (encodeStateMDNodes b) }

defineAttributeGroup :: A.A.GroupID -> FFI.FunctionAttributeSet -> EncodeAST ()
defineAttributeGroup gid attrs = modify $ \b -> b { encodeStateAttributeGroups = Map.insert gid attrs (encodeStateAttributeGroups b) }

defineCOMDAT :: ShortByteString -> Ptr FFI.COMDAT -> EncodeAST ()
defineCOMDAT name cd = modify $ \b -> b { encodeStateCOMDATs = Map.insert name cd (encodeStateCOMDATs b) }

refer :: (Show n, Ord n) => (EncodeState -> Map n v) -> n -> EncodeAST v -> EncodeAST v
refer r n f = do
  mop <- gets $ Map.lookup n . r
  maybe f return mop

undefinedReference :: Show n => String -> n -> EncodeAST a
undefinedReference m n = throwM . EncodeException $ "reference to undefined " ++ m ++ ": " ++ show n

referOrThrow :: (Show n, Ord n) => (EncodeState -> Map n v) -> String -> n -> EncodeAST v
referOrThrow r m n = refer r n $ undefinedReference m n

referGlobal :: A.Name -> EncodeAST (Ptr FFI.GlobalValue)
referGlobal = referOrThrow encodeStateGlobals "global"
referMDNode :: A.MetadataNodeID -> EncodeAST (Ptr FFI.MDNode)
referMDNode = referOrThrow encodeStateMDNodes "metadata node"
referAttributeGroup :: A.A.GroupID -> EncodeAST FFI.FunctionAttributeSet
referAttributeGroup = referOrThrow encodeStateAttributeGroups "attribute group"
referCOMDAT :: ShortByteString -> EncodeAST (Ptr FFI.COMDAT)
referCOMDAT = referOrThrow encodeStateCOMDATs "COMDAT"

defineBasicBlock :: A.Name -> A.Name -> Ptr FFI.BasicBlock -> EncodeAST ()
defineBasicBlock fn n b = modify $ \s -> s {
  encodeStateBlocks = Map.insert n b (encodeStateBlocks s),
  encodeStateAllBlocks = Map.insert (fn, n) b (encodeStateAllBlocks s)
}

instance EncodeM EncodeAST A.Name (Ptr FFI.BasicBlock) where
  encodeM = referOrThrow encodeStateBlocks "block"

getBlockForAddress :: A.Name -> A.Name -> EncodeAST (Ptr FFI.BasicBlock)
getBlockForAddress fn n = referOrThrow encodeStateAllBlocks "blockaddress" (fn, n)

