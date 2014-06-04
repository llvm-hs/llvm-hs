{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module LLVM.General.Internal.EncodeAST where

import Control.Exception
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.AnyCont

import Foreign.Ptr
import Foreign.C

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Builder as FFI
import qualified LLVM.General.Internal.FFI.Value as FFI

import qualified LLVM.General.AST as A

import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

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
      encodeStateNamedTypes :: Map A.Name (Ptr FFI.Type)
    }

newtype EncodeAST a = EncodeAST { unEncodeAST :: AnyContT (ErrorT String (StateT EncodeState IO)) a }
    deriving (
       Functor,
       Monad,
       MonadIO,
       MonadState EncodeState,
       MonadError String,
       MonadAnyCont IO,
       ScopeAnyCont
     )

lookupNamedType :: A.Name -> EncodeAST (Ptr FFI.Type)
lookupNamedType n = do
  t <- gets $ Map.lookup n . encodeStateNamedTypes
  maybe (fail $ "reference to undefined type: " ++ show n) return t

defineType :: A.Name -> Ptr FFI.Type -> EncodeAST ()
defineType n t = modify $ \s -> s { encodeStateNamedTypes = Map.insert n t (encodeStateNamedTypes s) }

runEncodeAST :: Context -> EncodeAST a -> ErrorT String IO a
runEncodeAST context@(Context ctx) (EncodeAST a) = ErrorT $ 
    bracket (FFI.createBuilderInContext ctx) FFI.disposeBuilder $ \builder -> do
      let initEncodeState = EncodeState { 
              encodeStateBuilder = builder,
              encodeStateContext = context,
              encodeStateLocals = Map.empty,
              encodeStateGlobals = Map.empty,
              encodeStateAllBlocks = Map.empty,
              encodeStateBlocks = Map.empty,
              encodeStateMDNodes = Map.empty,
              encodeStateNamedTypes = Map.empty
            }
      flip evalStateT initEncodeState . runErrorT . flip runAnyContT return $ a

withName :: A.Name -> (CString -> IO a) -> IO a
withName (A.Name n) = withCString n
withName (A.UnName _) = withCString ""

instance MonadAnyCont IO m => EncodeM m A.Name CString where
  encodeM (A.Name n) = encodeM n
  encodeM _ = encodeM ""

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
  def <- gets $ Map.lookup n . encodeStateLocals
  case def of
    Just (ForwardValue dummy) -> liftIO $ FFI.replaceAllUsesWith dummy v
    _ -> return ()
  modify $ \b -> b { encodeStateLocals = Map.insert n (DefinedValue v) (encodeStateLocals b) }

defineGlobal :: FFI.DescendentOf FFI.GlobalValue v => A.Name -> Ptr v -> EncodeAST ()
defineGlobal n v = modify $ \b -> b { encodeStateGlobals =  Map.insert n (FFI.upCast v) (encodeStateGlobals b) }

defineMDNode :: A.MetadataNodeID -> Ptr FFI.MDNode -> EncodeAST ()
defineMDNode n v = modify $ \b -> b { encodeStateMDNodes = Map.insert n (FFI.upCast v) (encodeStateMDNodes b) }

refer :: (Show n, Ord n) => (EncodeState -> Map n v) -> n -> EncodeAST v -> EncodeAST v
refer r n f = do
  mop <- gets $ Map.lookup n . r
  maybe f return mop

failAsUndefined :: Show n => String -> n -> EncodeAST a
failAsUndefined m n = fail $ "reference to undefined " ++ m ++ ": " ++ show n

referOrFail :: (Show n, Ord n) => (EncodeState -> Map n (Ptr p)) -> String -> n -> EncodeAST (Ptr p)
referOrFail r m n = refer r n $ failAsUndefined m n

referGlobal = referOrFail encodeStateGlobals "global"
referMDNode = referOrFail encodeStateMDNodes "metadata node"

defineBasicBlock :: A.Name -> A.Name -> Ptr FFI.BasicBlock -> EncodeAST ()
defineBasicBlock fn n b = modify $ \s -> s {
  encodeStateBlocks = Map.insert n b (encodeStateBlocks s),
  encodeStateAllBlocks = Map.insert (fn, n) b (encodeStateAllBlocks s)
}

instance EncodeM EncodeAST A.Name (Ptr FFI.BasicBlock) where
  encodeM = referOrFail encodeStateBlocks "block"

getBlockForAddress :: A.Name -> A.Name -> EncodeAST (Ptr FFI.BasicBlock)
getBlockForAddress fn n = referOrFail encodeStateAllBlocks "blockaddress" (fn, n)

