{-# LANGUAGE
  QuasiQuotes,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Type where

import LLVM.General.Prelude

import Control.Monad.State
import Control.Monad.AnyCont
import Control.Monad.Exceptable

import qualified Data.Set as Set

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import LLVM.General.Internal.FFI.LLVMCTypes (typeKindP)
import qualified LLVM.General.Internal.FFI.Type as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.AddrSpace as A

import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST

getStructure :: Ptr FFI.Type -> DecodeAST A.Type
getStructure t = scopeAnyCont $ do
  return A.StructureType
   `ap` (decodeM =<< liftIO (FFI.isPackedStruct t))
   `ap` do
       n <- liftIO (FFI.countStructElementTypes t)
       ts <- allocaArray n
       liftIO $ FFI.getStructElementTypes t ts
       decodeM (n, ts)

getStructDefinitions :: DecodeAST [A.Definition]
getStructDefinitions = do
  let getStructDefinition t = do
       opaque <- decodeM =<< liftIO (FFI.structIsOpaque t)
       if opaque then return Nothing else Just <$> getStructure t
  flip fix Set.empty $ \continue done -> do
    t <- takeTypeToDefine
    flip (maybe (return [])) t $ \t -> do
      if t `Set.member` done
        then
          continue done
        else
          return (:)
            `ap` (return A.TypeDefinition `ap` getTypeName t `ap` getStructDefinition t)
            `ap` (continue $ Set.insert t done)

isArrayType :: Ptr FFI.Type -> IO Bool
isArrayType t = do
  k <- liftIO $ FFI.getTypeKind t
  return $ k == FFI.typeKindArray

instance Monad m => EncodeM m A.AddrSpace FFI.AddrSpace where
  encodeM (A.AddrSpace a) = return FFI.AddrSpace `ap` encodeM a

instance Monad m => DecodeM m A.AddrSpace FFI.AddrSpace where
  decodeM (FFI.AddrSpace a) = return A.AddrSpace `ap` decodeM a

instance EncodeM EncodeAST A.Type (Ptr FFI.Type) where
  encodeM f = scopeAnyCont $ do
    Context context <- gets encodeStateContext
    case f of
      A.IntegerType bits -> do
        bits <- encodeM bits
        liftIO $ FFI.intTypeInContext context bits
      A.FunctionType returnTypeAST argTypeASTs isVarArg -> do
        returnType <- encodeM returnTypeAST
        argTypes <- encodeM argTypeASTs
        isVarArg <- encodeM isVarArg
        liftIO $ FFI.functionType returnType argTypes isVarArg
      A.PointerType elementType addressSpace -> do
        e <- encodeM elementType
        a <- encodeM addressSpace
        liftIO $ FFI.pointerType e a
      A.VoidType -> liftIO $ FFI.voidTypeInContext context
      A.FloatingPointType 16 A.IEEE -> liftIO $ FFI.halfTypeInContext context
      A.FloatingPointType 32 A.IEEE -> liftIO $ FFI.floatTypeInContext context
      A.FloatingPointType 64 A.IEEE -> liftIO $ FFI.doubleTypeInContext context
      A.FloatingPointType 80 A.DoubleExtended -> liftIO $ FFI.x86FP80TypeInContext context
      A.FloatingPointType 128 A.IEEE -> liftIO $ FFI.fP128TypeInContext context
      A.FloatingPointType 128 A.PairOfFloats -> liftIO $ FFI.ppcFP128TypeInContext context
      A.FloatingPointType _ _ -> throwError $ "unsupported floating point type: " ++ show f
      A.VectorType sz e -> do
        e <- encodeM e
        sz <- encodeM sz
        liftIO $ FFI.vectorType e sz
      A.ArrayType sz e -> do
        e <- encodeM e
        sz <- encodeM sz
        liftIO $ FFI.arrayType e sz
      A.StructureType packed ets -> do
        ets <- encodeM ets
        packed <- encodeM packed
        liftIO $ FFI.structTypeInContext context ets packed
      A.NamedTypeReference n -> lookupNamedType n
      A.MetadataType -> liftIO $ FFI.metadataTypeInContext context

instance DecodeM DecodeAST A.Type (Ptr FFI.Type) where
  decodeM t = scopeAnyCont $ do
    k <- liftIO $ FFI.getTypeKind t
    case k of
      [typeKindP|Void|] -> return A.VoidType
      [typeKindP|Integer|] -> A.IntegerType <$> (decodeM =<< liftIO (FFI.getIntTypeWidth t))
      [typeKindP|Function|] ->
          return A.FunctionType
               `ap` (decodeM =<< liftIO (FFI.getReturnType t))
               `ap` (do
                      n <- liftIO (FFI.countParamTypes t)
                      ts <- allocaArray n
                      liftIO $ FFI.getParamTypes t ts
                      decodeM (n, ts)
                   )
               `ap` (decodeM =<< liftIO (FFI.isFunctionVarArg t))
      [typeKindP|Pointer|] ->
          return A.PointerType
             `ap` (decodeM =<< liftIO (FFI.getElementType t))
             `ap` (decodeM =<< liftIO (FFI.getPointerAddressSpace t))
      [typeKindP|Half|] -> return $ A.FloatingPointType 16 A.IEEE
      [typeKindP|Float|] -> return $ A.FloatingPointType 32 A.IEEE
      [typeKindP|Double|] -> return $ A.FloatingPointType 64 A.IEEE
      [typeKindP|FP128|] -> return $ A.FloatingPointType 128 A.IEEE
      [typeKindP|X86_FP80|] -> return $ A.FloatingPointType 80 A.DoubleExtended
      [typeKindP|PPC_FP128|] -> return $ A.FloatingPointType 128 A.PairOfFloats
      [typeKindP|Vector|] ->
        return A.VectorType
         `ap` (decodeM =<< liftIO (FFI.getVectorSize t))
         `ap` (decodeM =<< liftIO (FFI.getElementType t))
      [typeKindP|Struct|] -> do
        let ifM c a b = c >>= \x -> if x then a else b
        ifM (decodeM =<< liftIO (FFI.structIsLiteral t))
            (getStructure t)
            (saveNamedType t >> return A.NamedTypeReference `ap` getTypeName t)
      [typeKindP|Metadata|] -> return A.MetadataType
      [typeKindP|Array|] ->
        return A.ArrayType
         `ap` (decodeM =<< liftIO (FFI.getArrayLength t))
         `ap` (decodeM =<< liftIO (FFI.getElementType t))
      _ -> error $ "unhandled type kind " ++ show k

createNamedType :: A.Name -> EncodeAST (Ptr FFI.Type)
createNamedType n = do
  Context c <- gets encodeStateContext
  n <- case n of { A.Name n -> encodeM n; _ -> return nullPtr }
  liftIO $ FFI.structCreateNamed c n


setNamedType :: Ptr FFI.Type -> A.Type -> EncodeAST ()
setNamedType t (A.StructureType packed ets) = do
  ets <- encodeM ets
  packed <- encodeM packed
  liftIO $ FFI.structSetBody t ets packed



