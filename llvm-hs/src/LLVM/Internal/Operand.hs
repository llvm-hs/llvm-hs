{-# LANGUAGE
  MultiParamTypeClasses,
  OverloadedStrings
  #-}
module LLVM.Internal.Operand where

import LLVM.Prelude

import Control.Monad.State
import Control.Monad.AnyCont
import qualified Data.Map as Map

import Foreign.Ptr
import Foreign.C

import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.InlineAssembly ()
import LLVM.Internal.Metadata ()

import Text.Printf
import qualified Debug.Trace as Debug

import qualified LLVM.AST as A

instance DecodeM DecodeAST A.Operand (Ptr FFI.Value) where
  decodeM v = do
    c <- liftIO $ FFI.isAConstant v
    if (c /= nullPtr) 
     then
      return A.ConstantOperand `ap` decodeM c
     else
      do m <- liftIO $ FFI.isAMetadataOperand v
         if (m /= nullPtr)
            then A.MetadataOperand <$> decodeM m
            else return A.LocalReference
                           `ap` (decodeM =<< (liftIO $ FFI.typeOf v))
                           `ap` getLocalName v

instance DecodeM DecodeAST A.Metadata (Ptr FFI.Metadata) where
  decodeM md = do
    s <- liftIO $ FFI.isAMDString md
    Debug.traceM "Here!!! DecodeAST A.Metadata (Ptr FFI.Metadata)"
    if (s /= nullPtr)
       then A.MDString <$> decodeM s
       else do difile <- liftIO $ FFI.isDIFile md
               if difile /= nullPtr
                 then A.MDNode . A.MetadataNodeSpecialized <$> decodeM difile
                 else do  n <- liftIO $ FFI.isAMDNode md
                          if (n /= nullPtr)
                              then A.MDNode <$> decodeM n
                              else do v <- liftIO $ FFI.isAMDValue md
                                      if (v /= nullPtr)
                                          then A.MDValue <$> decodeM v
                                          else fail "Metadata was not one of [MDString, MDValue, MDNode]"

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
  encodeM (A.MetadataOperand md) = do
    md' <- encodeM md
    Context c <- gets encodeStateContext
    liftIO $ FFI.upCast <$> FFI.metadataOperand c md'

instance EncodeM EncodeAST A.Metadata (Ptr FFI.Metadata) where
  encodeM (A.MDString s) = do
    Context c <- gets encodeStateContext
    s <- encodeM s
    liftM FFI.upCast $ liftIO $ FFI.mdStringInContext c s
  encodeM (A.MDNode mdn) = (FFI.upCast :: Ptr FFI.MDNode -> Ptr FFI.Metadata) <$> encodeM mdn
  encodeM (A.MDValue v) = do
     v <- encodeM v
     liftIO $ FFI.upCast <$> FFI.mdValue v

instance EncodeM EncodeAST A.CallableOperand (Ptr FFI.Value) where
  encodeM (Right o) = encodeM o
  encodeM (Left i) = liftM (FFI.upCast :: Ptr FFI.InlineAsm -> Ptr FFI.Value) (encodeM i)

instance EncodeM EncodeAST A.MetadataNode (Ptr FFI.MDNode) where
  encodeM (A.MetadataNode ops) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    ops <- encodeM ops
    liftIO $ FFI.createMDNodeInContext c ops
  encodeM (A.MetadataNodeReference n) = referMDNode n
  encodeM (A.MetadataNodeSpecialized smetaNode) = encodeM smetaNode

instance EncodeM EncodeAST A.SMetaNode (Ptr FFI.MDNode) where
  encodeM (A.DIFile filename directory chksumKind chksum) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    fn <- encodeM filename
    dir <- encodeM directory
    chk <- encodeM chksumKind
    chs <- encodeM chksum
    liftIO $ FFI.createDIFile c fn dir chk chs

instance EncodeM EncodeAST A.DIFileChecksum CUInt where
  encodeM A.CSK_None = return 0
  encodeM A.CSK_MD5 = return 1
  encodeM A.CSK_SHA1 = return 2

instance DecodeM DecodeAST A.DIFileChecksum CUInt where
  decodeM 0 = return A.CSK_None
  decodeM 1 = return A.CSK_MD5
  decodeM 2 = return A.CSK_SHA1

instance DecodeM DecodeAST [Maybe A.Metadata] (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    Debug.traceM $ printf "DecodeAST [Maybe A.Metadata] (Ptr FFI.MDNode)"
    n <- liftIO $ FFI.getMDNodeNumOperands p
    ops <- allocaArray n
    liftIO $ FFI.getMDNodeOperands p ops
    decodeM (n, ops)

instance DecodeM DecodeAST A.Operand (Ptr FFI.MDValue) where
  decodeM = decodeM <=< liftIO . FFI.getMDValue

instance DecodeM DecodeAST A.Metadata (Ptr FFI.MetadataAsVal) where
  decodeM = decodeM <=< liftIO . FFI.getMetadataOperand

instance DecodeM DecodeAST A.SMetaNode (Ptr FFI.DIFile) where
  decodeM p = do
    Debug.traceM $ printf "DecodeAST A.SMetaNode (Ptr FFI.DIFile)"
    return $ A.DIFile {
      A.filename = "stub",
      A.directory = "dir/stuf",
      A.checksumKind = A.CSK_None,
      A.checksum = Nothing
    }

instance DecodeM DecodeAST A.MetadataNode (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do

    -- fl <- decodeM =<< liftIO (FFI.mdNodeIsFunctionLocal p)
    -- if fl
    --  then
    --    return A.MetadataNode `ap` decodeM p
    --  else
       Debug.traceM $ printf "DecodeAST A.MetadataNode (Ptr FFI.MDNode)"
       return A.MetadataNodeReference `ap` getMetadataNodeID p

getMetadataDefinitions :: DecodeAST [A.Definition]
getMetadataDefinitions = fix $ \continue -> do
  mdntd <- takeMetadataNodeToDefine
  Debug.traceM ("Here!!! getMetadataDefinitions " ++ show mdntd)
  flip (maybe (return [])) mdntd $ \(mid, p) -> do
    return (:)
      `ap` (return A.MetadataNodeDefinition `ap` return mid `ap` decodeM p)
      `ap` continue
