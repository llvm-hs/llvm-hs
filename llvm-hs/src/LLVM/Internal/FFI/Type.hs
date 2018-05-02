{-# LANGUAGE
  ForeignFunctionInterface
  #-}
-- | Functions for handling the LLVM types
module LLVM.Internal.FFI.Type where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.PtrHierarchy

-- | <http://llvm.org/doxygen/group__LLVMCCoreType.html#ga112756467f0988613faa6043d674d843>
foreign import ccall unsafe "LLVMGetTypeKind" getTypeKind ::
  Ptr Type -> IO TypeKind

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeInt.html#gadfb8ba2f605f0860a4bf2e3c480ab6a2>
foreign import ccall unsafe "LLVMGetIntTypeWidth" getIntTypeWidth ::
  Ptr Type -> IO (CUInt)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFunction.html#ga2970f0f4d9ee8a0f811f762fb2fa7f82>
foreign import ccall unsafe "LLVMIsFunctionVarArg" isFunctionVarArg ::
  Ptr Type -> IO LLVMBool

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFunction.html#gacfa4594cbff421733add602a413cae9f>
foreign import ccall unsafe "LLVMGetReturnType" getReturnType ::
  Ptr Type -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFunction.html#ga44fa41d22ed1f589b8202272f54aad77>
foreign import ccall unsafe "LLVMCountParamTypes" countParamTypes ::
  Ptr Type -> IO CUInt

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFunction.html#ga83dd3a49a0f3f017f4233fc0d667bda2>
foreign import ccall unsafe "LLVMGetParamTypes" getParamTypes ::
  Ptr Type -> Ptr (Ptr Type) -> IO ()

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#ga0b03e26a2d254530a9b5c279cdf52257>
foreign import ccall unsafe "LLVMGetElementType" getElementType ::
  Ptr Type -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeInt.html#ga2e5db8cbc30daa156083f2c42989138d>
foreign import ccall unsafe "LLVMIntTypeInContext" intTypeInContext ::
  Ptr Context -> CUInt -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFunction.html#ga8b0c32e7322e5c6c1bf7eb95b0961707>
foreign import ccall unsafe "LLVMFunctionType" functionType' ::
  Ptr Type -> Ptr (Ptr Type) -> CUInt -> LLVMBool -> IO (Ptr Type)

functionType :: Ptr Type -> (CUInt, Ptr (Ptr Type)) -> LLVMBool -> IO (Ptr Type)
functionType rt (n, ats) va = functionType' rt ats n va

newtype AddrSpace = AddrSpace CUInt
-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#ga299fe6147083678d0494b1b875f542fae>
foreign import ccall unsafe "LLVMPointerType" pointerType ::
  Ptr Type -> AddrSpace -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#ga124b162b69b5def41dde2fda3668cbd9>
foreign import ccall unsafe "LLVMGetPointerAddressSpace" getPointerAddressSpace ::
  Ptr Type -> IO AddrSpace

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#ga5ec731adf74fb40bc3b401956d0c6ff2>
foreign import ccall unsafe "LLVMVectorType" vectorType ::
  Ptr Type -> CUInt -> IO (Ptr Type)

-- | what <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#gabd1666e080f693e1af0b4018005cd927>
-- | would be if it supported 64-bit array sizes, as the C++ type does.
foreign import ccall unsafe "LLVM_Hs_ArrayType" arrayType ::
  Ptr Type -> Word64 -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeStruct.html#gaff2af74740a22f7d18701f0d8c3e5a6f>
foreign import ccall unsafe "LLVMStructTypeInContext" structTypeInContext' ::
  Ptr Context -> Ptr (Ptr Type) -> CUInt -> LLVMBool -> IO (Ptr Type)

structTypeInContext :: Ptr Context -> (CUInt, Ptr (Ptr Type)) -> LLVMBool -> IO (Ptr Type)
structTypeInContext ctx (n, ts) p = structTypeInContext' ctx ts n p

foreign import ccall unsafe "LLVM_Hs_StructCreateNamed" structCreateNamed ::
  Ptr Context -> CString -> Ptr (OwnerTransfered CString) -> IO (Ptr Type)

foreign import ccall unsafe "LLVMGetStructName" getStructName ::
  Ptr Type -> IO CString

foreign import ccall unsafe "LLVM_Hs_StructIsLiteral" structIsLiteral ::
  Ptr Type -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_StructIsOpaque" structIsOpaque ::
  Ptr Type -> IO LLVMBool

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeStruct.html#ga3e940e660375ae0cbdde81c0d8ec91e3>
foreign import ccall unsafe "LLVMIsPackedStruct" isPackedStruct ::
  Ptr Type -> IO LLVMBool

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeStruct.html#gaf32e6d6bcec38b786efbef689b0dddf7>
foreign import ccall unsafe "LLVMCountStructElementTypes" countStructElementTypes ::
  Ptr Type -> IO CUInt

foreign import ccall unsafe "LLVMGetStructElementTypes" getStructElementTypes ::
  Ptr Type -> Ptr (Ptr Type) -> IO ()

foreign import ccall unsafe "LLVMStructSetBody" structSetBody' ::
  Ptr Type -> Ptr (Ptr Type) -> CUInt -> LLVMBool -> IO ()

structSetBody :: Ptr Type -> (CUInt, Ptr (Ptr Type)) -> LLVMBool -> IO ()
structSetBody s (n,ts) p = structSetBody' s ts n p

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#gafb88a5ebd2a8062e105854910dc7ca17>
foreign import ccall unsafe "LLVMGetVectorSize" getVectorSize ::
  Ptr Type -> IO CUInt

-- | what <http://llvm.org/doxygen/group__LLVMCCoreTypeSequential.html#ga02dc08041a12265cb700ee469497df63>
-- | would be if it supported 64 bit lengths
foreign import ccall unsafe "LLVM_Hs_GetArrayLength" getArrayLength ::
  Ptr Type -> IO Word64

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeOther.html#ga1c78ca6d7bf279330b9195fa52f23828>
foreign import ccall unsafe "LLVMVoidTypeInContext" voidTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#ga3a5332a1d075602bccad7576d1a8e36f>
foreign import ccall unsafe "LLVMHalfTypeInContext" halfTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#ga529c83a8a5461e5beac19eb867216e3c>
foreign import ccall unsafe "LLVMFloatTypeInContext" floatTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#ga200527010747eab31b73d3e3f6d94935>
foreign import ccall unsafe "LLVMDoubleTypeInContext" doubleTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#ga24f77b84b625ed3dd516b52480606093>
foreign import ccall unsafe "LLVMX86FP80TypeInContext" x86FP80TypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#ga1c02fb08f9ae12a719ed42099d42ccd8>
foreign import ccall unsafe "LLVMFP128TypeInContext" fP128TypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeFloat.html#gac2491184fc3d8631c7b264c067f2f761>
foreign import ccall unsafe "LLVMPPCFP128TypeInContext" ppcFP128TypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/classllvm_1_1Type.html#a28fdf240b8220065bc60d6d1b1a2f174>
foreign import ccall unsafe "LLVM_Hs_MetadataTypeInContext" metadataTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/docs/doxygen/html/Core_8cpp.html#a5d3702e198e2373db7e31bb18879efc3>
foreign import ccall unsafe "LLVM_Hs_TokenTypeInContext" tokenTypeInContext ::
  Ptr Context -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreTypeOther.html#ga7b7c56bf8406c50205fdd410b351ad81>
foreign import ccall unsafe "LLVMLabelTypeInContext" labelTypeInContext ::
  Ptr Context -> IO (Ptr Type)
