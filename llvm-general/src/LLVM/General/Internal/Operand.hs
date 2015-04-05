{-# LANGUAGE
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Operand where

import LLVM.General.Prelude

import Control.Monad.State
import Control.Monad.AnyCont
import qualified Data.Map as Map

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.Constant as FFI
import qualified LLVM.General.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.General.Internal.FFI.Metadata as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Value as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.Constant ()
import LLVM.General.Internal.Context
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.InlineAssembly ()
import LLVM.General.Internal.Metadata ()

import qualified LLVM.General.AST as A

instance DecodeM DecodeAST A.Operand (Ptr FFI.Value) where
  decodeM v = do
    c <- liftIO $ FFI.isAConstant v
    if (c /= nullPtr) 
     then
      return A.ConstantOperand `ap` decodeM c
     else
      do
        mds <- liftIO $ FFI.isAMDString v
        if mds /= nullPtr 
         then return A.MetadataStringOperand `ap` decodeM mds
         else
           do
             mdn <- liftIO $ FFI.isAMDNode v
             if mdn /= nullPtr
              then return A.MetadataNodeOperand `ap` decodeM mdn
              else
                return A.LocalReference 
                         `ap` (decodeM =<< (liftIO $ FFI.typeOf v))
                         `ap` getLocalName v

instance DecodeM DecodeAST A.CallableOperand (Ptr FFI.Value) where
  decodeM v = do
    ia <- liftIO $ FFI.isAInlineAsm v
    if ia /= nullPtr
     then liftM Left (decodeM ia)
     else liftM Right (decodeM v)

instance EncodeM EncodeAST A.Operand (Ptr FFI.Value) where
  encodeM (A.ConstantOperand c) = (FFI.upCast :: Ptr FFI.Constant -> Ptr FFI.Value) <$> encodeM c
  encodeM (A.LocalReference t n) = do
    lv <- refer encodeStateLocals n $ do
      lv <- do
        n <- encodeM n
        t <- encodeM t
        v <- liftIO $ FFI.createArgument t n
        return $ ForwardValue v
      modify $ \s -> s { encodeStateLocals = Map.insert n lv $ encodeStateLocals s }
      return lv
    return $ case lv of DefinedValue v -> v; ForwardValue v -> v

  encodeM (A.MetadataStringOperand s) = do
    Context c <- gets encodeStateContext
    s <- encodeM s
    liftM FFI.upCast $ liftIO $ FFI.mdStringInContext c s
  encodeM (A.MetadataNodeOperand mdn) = (FFI.upCast :: Ptr FFI.MDNode -> Ptr FFI.Value) <$> encodeM mdn

instance EncodeM EncodeAST A.CallableOperand (Ptr FFI.Value) where
  encodeM (Right o) = encodeM o
  encodeM (Left i) = liftM (FFI.upCast :: Ptr FFI.InlineAsm -> Ptr FFI.Value) (encodeM i)

instance EncodeM EncodeAST A.MetadataNode (Ptr FFI.MDNode) where
  encodeM (A.MetadataNode ops) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    ops <- encodeM ops
    liftIO $ FFI.createMDNodeInContext c ops
  encodeM (A.MetadataNodeReference n) = referMDNode n

instance DecodeM DecodeAST [Maybe A.Operand] (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    n <- liftIO $ FFI.getMDNodeNumOperands p
    ops <- allocaArray n
    liftIO $ FFI.getMDNodeOperands p ops
    decodeM (n, ops)

instance DecodeM DecodeAST A.MetadataNode (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    fl <- decodeM =<< liftIO (FFI.mdNodeIsFunctionLocal p)
    if fl
     then
       return A.MetadataNode `ap` decodeM p
     else
       return A.MetadataNodeReference `ap` getMetadataNodeID p

getMetadataDefinitions :: DecodeAST [A.Definition]
getMetadataDefinitions = fix $ \continue -> do
  mdntd <- takeMetadataNodeToDefine
  flip (maybe (return [])) mdntd $ \(mid, p) -> do
    return (:)
      `ap` (return A.MetadataNodeDefinition `ap` return mid `ap` decodeM p)
      `ap` continue
