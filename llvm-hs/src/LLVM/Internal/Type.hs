{-# LANGUAGE
  QuasiQuotes,
  MultiParamTypeClasses
  #-}
module LLVM.Internal.Type where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Foreign.Ptr

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import LLVM.Internal.FFI.LLVMCTypes (typeKindP)
import qualified LLVM.Internal.FFI.Type as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.AST as A
import qualified LLVM.AST.AddrSpace as A
import LLVM.Exception

import LLVM.Internal.Context
import LLVM.Internal.Coding
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST

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
      A.FloatingPointType A.HalfFP      -> liftIO $ FFI.halfTypeInContext context
      A.FloatingPointType A.FloatFP     -> liftIO $ FFI.floatTypeInContext context
      A.FloatingPointType A.DoubleFP    -> liftIO $ FFI.doubleTypeInContext context
      A.FloatingPointType A.X86_FP80FP  -> liftIO $ FFI.x86FP80TypeInContext context
      A.FloatingPointType A.FP128FP     -> liftIO $ FFI.fP128TypeInContext context
      A.FloatingPointType A.PPC_FP128FP -> liftIO $ FFI.ppcFP128TypeInContext context
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
      A.TokenType -> liftIO $ FFI.tokenTypeInContext context
      A.LabelType -> liftIO $ FFI.labelTypeInContext context

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
      [typeKindP|Half|]      -> return $ A.FloatingPointType A.HalfFP
      [typeKindP|Float|]     -> return $ A.FloatingPointType A.FloatFP
      [typeKindP|Double|]    -> return $ A.FloatingPointType A.DoubleFP
      [typeKindP|FP128|]     -> return $ A.FloatingPointType A.FP128FP
      [typeKindP|X86_FP80|]  -> return $ A.FloatingPointType A.X86_FP80FP
      [typeKindP|PPC_FP128|] -> return $ A.FloatingPointType A.PPC_FP128FP
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
      [typeKindP|Token|] -> return A.TokenType
      [typeKindP|Label|] -> return A.LabelType
      _ -> error $ "unhandled type kind " ++ show k

createNamedType :: A.Name -> EncodeAST (Ptr FFI.Type, Maybe ShortByteString)
createNamedType n = do
  Context c <- gets encodeStateContext
  n <- case n of { A.Name n -> encodeM n; _ -> return nullPtr }
  renamedName <- alloca
  t <- liftIO $ FFI.structCreateNamed c n renamedName
  p <- peek renamedName
  if p == FFI.OwnerTransfered nullPtr
    then pure (t, Nothing)
    else do
      n' <- decodeM p
      pure (t, Just n')

renameType :: A.Type -> EncodeAST A.Type
renameType A.VoidType = pure A.VoidType
renameType t@(A.IntegerType _) = pure t
renameType (A.PointerType r a) = fmap (\r' -> A.PointerType r' a) (renameType r)
renameType t@(A.FloatingPointType _) = pure t
renameType (A.FunctionType r as varArg) =
  liftA2
    (\r' as' -> A.FunctionType r' as' varArg)
    (renameType r)
    (traverse renameType as)
renameType (A.VectorType n t) = A.VectorType n <$> renameType t
renameType (A.StructureType packed ts) = A.StructureType packed <$> traverse renameType ts
renameType (A.ArrayType n t) = A.ArrayType n <$> renameType t
renameType t@(A.NamedTypeReference n) = do
  renamedTypes <- gets encodeStateRenamedTypes
  case Map.lookup n renamedTypes of
    Just n' -> pure (A.NamedTypeReference (A.Name n'))
    Nothing -> pure t
renameType A.MetadataType = pure A.MetadataType
renameType A.LabelType = pure A.LabelType
renameType A.TokenType = pure A.TokenType

setNamedType :: Ptr FFI.Type -> A.Type -> EncodeAST ()
setNamedType t (A.StructureType packed ets) = do
  ets <- encodeM ets
  packed <- encodeM packed
  liftIO $ FFI.structSetBody t ets packed
setNamedType _ ty =
  throwM
    (EncodeException
       ("A type definition requires a structure type but got: " ++ show ty))
