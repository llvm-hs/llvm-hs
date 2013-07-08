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

import qualified LLVM.General.AST as A

import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

data EncodeState = EncodeState { 
      encodeStateBuilder :: Ptr FFI.Builder,
      encodeStateContext :: Context,
      encodeStateLocals :: Map A.Name (Ptr FFI.Value),
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

define :: (Ord n, FFI.DescendentOf p v) => 
          (EncodeState -> Map n (Ptr p))
          -> (Map n (Ptr p) -> EncodeState -> EncodeState)
          -> n
          -> Ptr v
          -> EncodeAST ()
define r w n v = modify $ \b -> w (Map.insert n (FFI.upCast v) (r b)) b

defineLocal :: FFI.DescendentOf FFI.Value v => A.Name -> Ptr v -> EncodeAST ()
defineLocal = define encodeStateLocals (\m b -> b { encodeStateLocals = m })

defineGlobal :: FFI.DescendentOf FFI.GlobalValue v => A.Name -> Ptr v -> EncodeAST ()
defineGlobal = define encodeStateGlobals (\m b -> b { encodeStateGlobals = m })

defineMDNode :: A.MetadataNodeID -> Ptr FFI.MDNode -> EncodeAST ()
defineMDNode = define encodeStateMDNodes (\m b -> b { encodeStateMDNodes = m })

refer :: (Show n, Ord n) => (EncodeState -> Map n (Ptr p)) -> String -> n -> EncodeAST (Ptr p)
refer r m n = do
  mop <- gets $ Map.lookup n . r
  maybe (fail $ "reference to undefined " ++ m ++ ": " ++ show n) return mop

referLocal = refer encodeStateLocals "local"
referGlobal = refer encodeStateGlobals "global"
referMDNode = refer encodeStateMDNodes "metadata node"

defineBasicBlock :: A.Name -> A.Name -> Ptr FFI.BasicBlock -> EncodeAST ()
defineBasicBlock fn n b = modify $ \s -> s {
  encodeStateBlocks = Map.insert n b (encodeStateBlocks s),
  encodeStateAllBlocks = Map.insert (fn, n) b (encodeStateAllBlocks s)
}

instance EncodeM EncodeAST A.Name (Ptr FFI.BasicBlock) where
  encodeM = refer encodeStateBlocks "block"

getBlockForAddress :: A.Name -> A.Name -> EncodeAST (Ptr FFI.BasicBlock)
getBlockForAddress fn n = refer encodeStateAllBlocks "blockaddress" (fn, n)

