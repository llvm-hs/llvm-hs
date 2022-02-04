{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module LLVM.Internal.Attribute where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Control.Exception
import Foreign.C (CUInt)
import Foreign.Ptr
import Data.Maybe

import qualified LLVM.Internal.FFI.Attribute as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import LLVM.Internal.FFI.LLVMCTypes (parameterAttributeKindP, functionAttributeKindP)

import qualified LLVM.AST.ParameterAttribute as A.PA
import qualified LLVM.AST.FunctionAttribute as A.FA

import LLVM.Internal.Coding
import LLVM.Internal.Context
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST

inconsistentCases :: Show a => String -> a -> b
inconsistentCases name attr =
  error $ "llvm-hs internal error: cases inconstistent in " ++ name ++ " encoding for " ++ show attr

instance Monad m => EncodeM m A.PA.ParameterAttribute (Ptr FFI.ParameterAttrBuilder -> EncodeAST ()) where
  encodeM (A.PA.StringAttribute kind value) = return $ \b -> do
    (kindP, kindLen) <- encodeM kind
    (valueP, valueLen) <- encodeM value
    liftIO $ FFI.attrBuilderAddStringAttribute b kindP kindLen valueP valueLen
  encodeM a = return $ \b -> liftIO $ case a of
    A.PA.Alignment v -> FFI.attrBuilderAddAlignment b v
    A.PA.Dereferenceable v -> FFI.attrBuilderAddDereferenceable b v
    A.PA.DereferenceableOrNull v -> FFI.attrBuilderAddDereferenceableOrNull b v
    _ -> FFI.attrBuilderAddParameterAttributeKind b $ case a of
      A.PA.ByVal -> FFI.parameterAttributeKindByVal
      A.PA.ImmArg -> FFI.parameterAttributeKindImmArg
      A.PA.InAlloca -> FFI.parameterAttributeKindInAlloca
      A.PA.InReg -> FFI.parameterAttributeKindInReg
      A.PA.Nest -> FFI.parameterAttributeKindNest
      A.PA.NoAlias -> FFI.parameterAttributeKindNoAlias
      A.PA.NoCapture -> FFI.parameterAttributeKindNoCapture
      A.PA.NoFree -> FFI.parameterAttributeKindNoFree
      A.PA.NonNull -> FFI.parameterAttributeKindNonNull
      A.PA.ReadNone -> FFI.parameterAttributeKindReadNone
      A.PA.ReadOnly -> FFI.parameterAttributeKindReadOnly
      A.PA.Returned -> FFI.parameterAttributeKindReturned
      A.PA.SignExt -> FFI.parameterAttributeKindSExt
      A.PA.SRet -> FFI.parameterAttributeKindStructRet
      A.PA.SwiftError -> FFI.parameterAttributeKindSwiftError
      A.PA.SwiftSelf -> FFI.parameterAttributeKindSwiftSelf
      A.PA.WriteOnly -> FFI.parameterAttributeKindWriteOnly
      A.PA.ZeroExt -> FFI.parameterAttributeKindZExt
#if __GLASGOW_HASKELL__ < 900
      A.PA.Alignment _ -> inconsistentCases "ParameterAttribute" a
      A.PA.Dereferenceable _ -> inconsistentCases "ParameterAttribute" a
      A.PA.DereferenceableOrNull _ -> inconsistentCases "ParameterAttribute" a
      A.PA.StringAttribute _ _ -> inconsistentCases "ParameterAttribute" a
#endif

instance Monad m => EncodeM m A.FA.FunctionAttribute (Ptr FFI.FunctionAttrBuilder -> EncodeAST ()) where
  encodeM (A.FA.StringAttribute kind value) = return $ \b -> do
    (kindP, kindLen) <- encodeM kind
    (valueP, valueLen) <- encodeM value
    liftIO $ FFI.attrBuilderAddStringAttribute b kindP kindLen valueP valueLen
  encodeM a = return $ \b -> case a of
    A.FA.AllocSize x y -> do
      x' <- encodeM x
      y' <- encodeM y
      liftIO $ FFI.attrBuilderAddAllocSize b x' y'
    A.FA.StackAlignment v -> liftIO $ FFI.attrBuilderAddStackAlignment b v
    _ -> liftIO $ FFI.attrBuilderAddFunctionAttributeKind b $ case a of
      A.FA.AlwaysInline -> FFI.functionAttributeKindAlwaysInline
      A.FA.ArgMemOnly -> FFI.functionAttributeKindArgMemOnly
      A.FA.Builtin -> FFI.functionAttributeKindBuiltin
      A.FA.Cold -> FFI.functionAttributeKindCold
      A.FA.Convergent -> FFI.functionAttributeKindConvergent
      A.FA.InaccessibleMemOnly -> FFI.functionAttributeKindInaccessibleMemOnly
      A.FA.InaccessibleMemOrArgMemOnly -> FFI.functionAttributeKindInaccessibleMemOrArgMemOnly
      A.FA.InlineHint -> FFI.functionAttributeKindInlineHint
      A.FA.JumpTable -> FFI.functionAttributeKindJumpTable
      A.FA.MinimizeSize -> FFI.functionAttributeKindMinSize
      A.FA.MustProgress -> FFI.functionAttributeKindMustProgress
      A.FA.Naked -> FFI.functionAttributeKindNaked
      A.FA.NoBuiltin -> FFI.functionAttributeKindNoBuiltin
      A.FA.NoDuplicate -> FFI.functionAttributeKindNoDuplicate
      A.FA.NoFree -> FFI.functionAttributeKindNoFree
      A.FA.NoImplicitFloat -> FFI.functionAttributeKindNoImplicitFloat
      A.FA.NoInline -> FFI.functionAttributeKindNoInline
      A.FA.NonLazyBind -> FFI.functionAttributeKindNonLazyBind
      A.FA.NoRecurse -> FFI.functionAttributeKindNoRecurse
      A.FA.NoRedZone -> FFI.functionAttributeKindNoRedZone
      A.FA.NoReturn -> FFI.functionAttributeKindNoReturn
      A.FA.NoSync -> FFI.functionAttributeKindNoSync
      A.FA.NoUnwind -> FFI.functionAttributeKindNoUnwind
      A.FA.OptimizeForSize -> FFI.functionAttributeKindOptimizeForSize
      A.FA.OptimizeNone -> FFI.functionAttributeKindOptimizeNone
      A.FA.ReadNone -> FFI.functionAttributeKindReadNone
      A.FA.ReadOnly -> FFI.functionAttributeKindReadOnly
      A.FA.ReturnsTwice -> FFI.functionAttributeKindReturnsTwice
      A.FA.SafeStack -> FFI.functionAttributeKindSafeStack
      A.FA.SanitizeAddress -> FFI.functionAttributeKindSanitizeAddress
      A.FA.SanitizeHWAddress -> FFI.functionAttributeKindSanitizeHWAddress
      A.FA.SanitizeMemory -> FFI.functionAttributeKindSanitizeMemory
      A.FA.SanitizeThread -> FFI.functionAttributeKindSanitizeThread
      A.FA.Speculatable -> FFI.functionAttributeKindSpeculatable
      A.FA.StackProtect -> FFI.functionAttributeKindStackProtect
      A.FA.StackProtectReq -> FFI.functionAttributeKindStackProtectReq
      A.FA.StackProtectStrong -> FFI.functionAttributeKindStackProtectStrong
      A.FA.StrictFP -> FFI.functionAttributeKindStrictFP
      A.FA.UWTable -> FFI.functionAttributeKindUWTable
      A.FA.WillReturn -> FFI.functionAttributeKindWillReturn
      A.FA.WriteOnly -> FFI.functionAttributeKindWriteOnly
#if __GLASGOW_HASKELL__ < 900
      A.FA.AllocSize _ _ -> inconsistentCases "FunctionAttribute" a
      A.FA.StackAlignment _ -> inconsistentCases "FunctionAttribute" a
      A.FA.StringAttribute _ _ -> inconsistentCases "FunctionAttribute" a
#endif

instance DecodeM DecodeAST A.PA.ParameterAttribute FFI.ParameterAttribute where
  decodeM a = do
    isString <- decodeM =<< liftIO (FFI.isStringAttribute a)
    if isString
      then
        A.PA.StringAttribute
          <$> decodeM (FFI.attributeKindAsString a)
          <*> decodeM (FFI.attributeValueAsString a)
      else do
        enum <- liftIO $ FFI.parameterAttributeKindAsEnum a
        case enum of
          [parameterAttributeKindP|Alignment|] -> return A.PA.Alignment `ap` (liftIO $ FFI.attributeValueAsInt a)
          [parameterAttributeKindP|ByVal|] -> return A.PA.ByVal
          [parameterAttributeKindP|DereferenceableOrNull|] -> return A.PA.DereferenceableOrNull `ap` (liftIO $ FFI.attributeValueAsInt a)
          [parameterAttributeKindP|Dereferenceable|] -> return A.PA.Dereferenceable `ap` (liftIO $ FFI.attributeValueAsInt a)
          [parameterAttributeKindP|ImmArg|] -> return A.PA.ImmArg
          [parameterAttributeKindP|InAlloca|] -> return A.PA.InAlloca
          [parameterAttributeKindP|InReg|] -> return A.PA.InReg
          [parameterAttributeKindP|Nest|] -> return A.PA.Nest
          [parameterAttributeKindP|NoAlias|] -> return A.PA.NoAlias
          [parameterAttributeKindP|NoCapture|] -> return A.PA.NoCapture
          [parameterAttributeKindP|NoFree|] -> return A.PA.NoFree
          [parameterAttributeKindP|NonNull|] -> return A.PA.NonNull
          [parameterAttributeKindP|ReadNone|] -> return A.PA.ReadNone
          [parameterAttributeKindP|ReadOnly|] -> return A.PA.ReadOnly
          [parameterAttributeKindP|Returned|] -> return A.PA.Returned
          [parameterAttributeKindP|SExt|] -> return A.PA.SignExt
          [parameterAttributeKindP|StructRet|] -> return A.PA.SRet
          [parameterAttributeKindP|SwiftError|] -> return A.PA.SwiftError
          [parameterAttributeKindP|SwiftSelf|] -> return A.PA.SwiftSelf
          [parameterAttributeKindP|WriteOnly|] -> return A.PA.WriteOnly
          [parameterAttributeKindP|ZExt|] -> return A.PA.ZeroExt
          _ -> error $ "unhandled parameter attribute enum value: " ++ show enum

instance DecodeM DecodeAST A.FA.FunctionAttribute FFI.FunctionAttribute where
  decodeM a = do
    isString <- decodeM =<< (liftIO $ FFI.isStringAttribute a)
    if isString
       then
         return A.FA.StringAttribute
                  `ap` (decodeM $ FFI.attributeKindAsString a)
                  `ap` (decodeM $ FFI.attributeValueAsString a)
       else do
         enum <- liftIO $ FFI.functionAttributeKindAsEnum a
         case enum of
           [functionAttributeKindP|AllocSize|] -> do
             x <- alloca
             y <- decodeOptional (FFI.attributeGetAllocSizeArgs a x)
             x' <- decodeM =<< peek x
             return (A.FA.AllocSize x' y)
           [functionAttributeKindP|AlwaysInline|] -> return A.FA.AlwaysInline
           [functionAttributeKindP|ArgMemOnly|] -> return A.FA.ArgMemOnly
           [functionAttributeKindP|Builtin|] -> return A.FA.Builtin
           [functionAttributeKindP|Cold|] -> return A.FA.Cold
           [functionAttributeKindP|Convergent|] -> return A.FA.Convergent
           [functionAttributeKindP|InaccessibleMemOnly|] -> return A.FA.InaccessibleMemOnly
           [functionAttributeKindP|InaccessibleMemOrArgMemOnly|] -> return A.FA.InaccessibleMemOrArgMemOnly
           [functionAttributeKindP|InlineHint|] -> return A.FA.InlineHint
           [functionAttributeKindP|JumpTable|] -> return A.FA.JumpTable
           [functionAttributeKindP|MinSize|] -> return A.FA.MinimizeSize
           [functionAttributeKindP|MustProgress|] -> return A.FA.MustProgress
           [functionAttributeKindP|Naked|] -> return A.FA.Naked
           [functionAttributeKindP|NoBuiltin|] -> return A.FA.NoBuiltin
           [functionAttributeKindP|NoDuplicate|] -> return A.FA.NoDuplicate
           [functionAttributeKindP|NoFree|] -> return A.FA.NoFree
           [functionAttributeKindP|NoImplicitFloat|] -> return A.FA.NoImplicitFloat
           [functionAttributeKindP|NoInline|] -> return A.FA.NoInline
           [functionAttributeKindP|NoRecurse|] -> return A.FA.NoRecurse
           [functionAttributeKindP|NoRedZone|] -> return A.FA.NoRedZone
           [functionAttributeKindP|NoReturn|] -> return A.FA.NoReturn
           [functionAttributeKindP|NoSync|] -> return A.FA.NoSync
           [functionAttributeKindP|NoUnwind|] -> return A.FA.NoUnwind
           [functionAttributeKindP|NonLazyBind|] -> return A.FA.NonLazyBind
           [functionAttributeKindP|OptimizeForSize|] -> return A.FA.OptimizeForSize
           [functionAttributeKindP|OptimizeNone|] -> return A.FA.OptimizeNone
           [functionAttributeKindP|ReadNone|] -> return A.FA.ReadNone
           [functionAttributeKindP|ReadOnly|] -> return A.FA.ReadOnly
           [functionAttributeKindP|ReturnsTwice|] -> return A.FA.ReturnsTwice
           [functionAttributeKindP|SafeStack|] -> return A.FA.SafeStack
           [functionAttributeKindP|SanitizeAddress|] -> return A.FA.SanitizeAddress
           [functionAttributeKindP|SanitizeHWAddress|] -> return A.FA.SanitizeHWAddress
           [functionAttributeKindP|SanitizeMemory|] -> return A.FA.SanitizeMemory
           [functionAttributeKindP|SanitizeThread|] -> return A.FA.SanitizeThread
           [functionAttributeKindP|Speculatable|] -> return A.FA.Speculatable
           [functionAttributeKindP|StackAlignment|] -> return A.FA.StackAlignment `ap` (liftIO $ FFI.attributeValueAsInt a)
           [functionAttributeKindP|StackProtectReq|] -> return A.FA.StackProtectReq
           [functionAttributeKindP|StackProtectStrong|] -> return A.FA.StackProtectStrong
           [functionAttributeKindP|StackProtect|] -> return A.FA.StackProtect
           [functionAttributeKindP|StrictFP|] -> return A.FA.StrictFP
           [functionAttributeKindP|UWTable|] -> return A.FA.UWTable
           [functionAttributeKindP|WillReturn|] -> return A.FA.WillReturn
           [functionAttributeKindP|WriteOnly|] -> return A.FA.WriteOnly
           _ -> error $ "unhandled function attribute enum value: " ++ show enum

allocaAttrBuilder :: (Monad m, MonadAnyCont IO m) => m (Ptr (FFI.AttrBuilder a))
allocaAttrBuilder = do
  p <- allocaArray FFI.getAttrBuilderSize
  anyContToM $ \f -> do
    ab <- FFI.constructAttrBuilder p
    r <- f ab
    FFI.destroyAttrBuilder ab
    return r

instance forall a b. EncodeM EncodeAST a (Ptr (FFI.AttrBuilder b) -> EncodeAST ()) =>
         EncodeM EncodeAST [a] (FFI.AttributeSet b) where
  encodeM as = do
    ab <- allocaAttrBuilder
    builds <- mapM encodeM as
    void (forM builds ($ ab) :: EncodeAST [()])
    Context context <- gets encodeStateContext
    anyContToM
      (bracket (FFI.getAttributeSet context ab) FFI.disposeAttributeSet)

instance forall a b. DecodeM DecodeAST a (FFI.Attribute b) => DecodeM DecodeAST [a] (FFI.AttributeSet b) where
  decodeM as = do
    numAttributes <- liftIO (FFI.getNumAttributes as)
    attrs <- allocaArray numAttributes
    liftIO (FFI.getAttributes as attrs)
    decodeM (numAttributes, attrs :: Ptr (FFI.Attribute b))

data AttributeList = AttributeList {
    functionAttributes :: [Either A.FA.GroupID A.FA.FunctionAttribute],
    returnAttributes :: [A.PA.ParameterAttribute],
    parameterAttributes :: [[A.PA.ParameterAttribute]]
  }
  deriving (Eq, Show)

data PreSlot
  = IndirectFunctionAttributes A.FA.GroupID
  | DirectFunctionAttributes [A.FA.FunctionAttribute]
  | ReturnAttributes [A.PA.ParameterAttribute]
  | ParameterAttributes CUInt [A.PA.ParameterAttribute]

instance {-# OVERLAPPING #-} EncodeM EncodeAST [Either A.FA.GroupID A.FA.FunctionAttribute] FFI.FunctionAttributeSet where
  encodeM attrs = do
    ab <- allocaAttrBuilder
    forM_ attrs $ \attr ->
      case attr of
        Left groupId -> do
          attrSet <- referAttributeGroup groupId
          ab' <- anyContToM (bracket (FFI.attrBuilderFromSet attrSet) FFI.disposeAttrBuilder)
          liftIO (FFI.mergeAttrBuilder ab ab')
        Right attr -> do
          addAttr <- encodeM attr
          addAttr ab :: EncodeAST ()
    Context context <- gets encodeStateContext
    anyContToM
      (bracket (FFI.getAttributeSet context ab) FFI.disposeAttributeSet)

instance EncodeM EncodeAST AttributeList FFI.AttributeList where
  encodeM (AttributeList fAttrs rAttrs pAttrs) = do
    fAttrSet <- encodeM fAttrs
    rAttrSet <- encodeM rAttrs :: EncodeAST FFI.ParameterAttributeSet
    (numPAttrs, pAttrSets) <- encodeM pAttrs
    Context context <- gets encodeStateContext
    anyContToM
      (bracket
         (FFI.buildAttributeList context fAttrSet rAttrSet pAttrSets numPAttrs)
         FFI.disposeAttributeList)

instance DecodeM DecodeAST AttributeList (FFI.AttrSetDecoder a, a) where
  decodeM (FFI.AttrSetDecoder attrsAtIndex countParams, a) = do
    functionAttrSet <-
      do mAttrSet <-
           -- function attributes are grouped and decoded later. Since
           -- we are sometimes decoding inside of scopeAnyConT, we
           -- cannot use withAttrsAtIndex to allocate the attribute
           -- set since it will be freed before we decode it.
           liftIO . mask_ $ do
             attrSet <-
               attrsAtIndex a FFI.functionIndex :: IO FFI.FunctionAttributeSet
             hasAttributes <- decodeM =<< FFI.attributeSetHasAttributes attrSet
             if hasAttributes
               then pure (Just attrSet)
               else FFI.disposeAttributeSet attrSet >> pure Nothing
         case mAttrSet of
           Nothing -> pure Nothing
           Just attrSet -> Just . Left <$> getAttributeGroupID attrSet
    returnAttrs <-
      do attrSet <-
           withAttrsAtIndex FFI.returnIndex :: DecodeAST FFI.ParameterAttributeSet
         decodeM attrSet
    numParams <- liftIO (countParams a)
    paramAttrs <-
      forM [1 .. numParams] $ \i ->
        decodeM =<<
        (withAttrsAtIndex (FFI.AttributeIndex i) :: DecodeAST FFI.ParameterAttributeSet)
    return
      (AttributeList
       { functionAttributes = maybeToList functionAttrSet
       , returnAttributes = returnAttrs
       , parameterAttributes = paramAttrs
       })
    where
      withAttrsAtIndex :: FFI.AttributeIndex -> DecodeAST (FFI.AttributeSet b)
      withAttrsAtIndex index =
        anyContToM (bracket (attrsAtIndex a index) (FFI.disposeAttributeSet))
